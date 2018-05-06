"""
MIDI messages

There is no need to use this module directly. All you need is
available in the top level module.
"""

from __future__ import division
import sys
import math
import struct
from contextlib import contextmanager

PY2 = (sys.version_info.major == 2)

# Pitchwheel is a 14 bit signed integer
MIN_PITCHWHEEL = -8192
MAX_PITCHWHEEL = 8191

# Song pos is a 14 bit unsigned integer
MIN_SONGPOS = 0
MAX_SONGPOS = 16383


class MessageSpec(object):
    """
    Specifications for creating a message.

    status_byte is the first byte of the message. For channel
    messages, the channel (lower 4 bits) is clear.

    type is the type name of the message, for example 'sysex'.

    arguments is the attributes / keywords arguments specific to
    this message type.

    length is the length of this message in bytes. This value is not used
    for sysex messages, since they use an end byte instead.

    Table of MIDI messages:

        http://www.midi.org/techspecs/midimessages.php
    """

    def __init__(self, status_byte, type_, arguments, length):
        """Create a new message specification."""
        self.status_byte = status_byte
        self.type = type_
        self.arguments = arguments
        self.length = length

        # Attributes that can be set on the object
        self.settable_attributes = set(self.arguments) | {'time'}

    def signature(self):
        """Return call signature for Message constructor for this type.

        The signature is returned as a string.
        """
        parts = []
        parts.append(repr(self.type))

        for name in self.arguments:
            if name == 'data':
                parts.append('data=()')
            else:
                parts.append('{}=0'.format(name))
        parts.append('time=0')

        sig = '({})'.format(', '.join(parts))

        return sig


def get_message_specs():
    return [
        # Channel messages
        MessageSpec(0x80, 'note_off', ('channel', 'note', 'velocity'), 3),
        MessageSpec(0x90, 'note_on', ('channel', 'note', 'velocity'), 3),
        MessageSpec(0xa0, 'polytouch', ('channel', 'note', 'value'), 3),
        MessageSpec(0xb0, 'control_change',
                    ('channel', 'control', 'value'), 3),
        MessageSpec(0xc0, 'program_change', ('channel', 'program',), 2),
        MessageSpec(0xd0, 'aftertouch', ('channel', 'value',), 2),
        MessageSpec(0xe0, 'pitchwheel', ('channel', 'pitch',), 3),

        # System common messages
        MessageSpec(0xf0, 'sysex', ('data',), float('inf')),
        MessageSpec(0xf1, 'quarter_frame', ('frame_type', 'frame_value'), 2),
        MessageSpec(0xf2, 'songpos', ('pos',), 3),
        MessageSpec(0xf3, 'song_select', ('song',), 2),
        # 0xf4 is undefined.
        # 0xf5 is undefined.
        MessageSpec(0xf6, 'tune_request', (), 1),
        # 0xf7 is the stop byte for sysex messages, so should not be a message.

        # System real time messages
        MessageSpec(0xf8, 'clock', (), 1),
        # 0xf9 is undefined.
        MessageSpec(0xfa, 'start', (), 1),
        MessageSpec(0xfb, 'continue', (), 1),
        MessageSpec(0xfc, 'stop', (), 1),
        # 0xfd is undefined.
        MessageSpec(0xfe, 'active_sensing', (), 1),
        MessageSpec(0xff, 'reset', (), 1),
    ]


def build_spec_lookup(message_specs):
    lookup = {}

    for spec in message_specs:
        status_byte = spec.status_byte

        if status_byte < 0xf0:
            # Channel message.
            # The upper 4 bits are message type, and
            # the lower 4 are MIDI channel.
            # We need lookup for all 16 MIDI channels.
            for channel in range(16):
                lookup[status_byte | channel] = spec
        else:
            lookup[status_byte] = spec

        lookup[spec.type] = spec

    return lookup


def check_time(time):
    """Check type and value of time.

    Raises TypeError if value is not an integer or a float
    """
    if PY2 and isinstance(time, long):
        return

    if not (isinstance(time, int) or isinstance(time, float)):
        raise TypeError('time must be an integer or float')


def check_channel(channel):
    """Check type and value of channel.

    Raises TypeError if the value is not an integer, and ValueError if
    it is outside range 0..127.
    """
    if not isinstance(channel, int):
        raise TypeError('channel must be an integer')
    elif not 0 <= channel <= 15:
        raise ValueError('channel must be in range 0..15')


def check_pos(pos):
    """Check type and value of song position.

    Raise TypeError if the value is not an integer, and ValueError if
    it is outside range MIN_SONGPOS..MAX_SONGPOS.
    """
    if not isinstance(pos, int):
        raise TypeError('song pos must be and integer')
    elif not MIN_SONGPOS <= pos <= MAX_SONGPOS:
        raise ValueError('song pos must be in range {}..{}'.format(
            MIN_SONGPOS, MAX_SONGPOS))


def check_pitch(pitch):
    """Raise TypeError if the value is not an integer, and ValueError
    if it is outside range MIN_PITCHWHEEL..MAX_PITCHWHEEL.
    """
    if not isinstance(pitch, int):
        raise TypeError('pichwheel value must be an integer')
    elif not MIN_PITCHWHEEL <= pitch <= MAX_PITCHWHEEL:
        raise ValueError('pitchwheel value must be in range {}..{}'.format(
            MIN_PITCHWHEEL, MAX_PITCHWHEEL))


def check_data(data_bytes):
    """Check type of data_byte and type and range of each data byte.

    Returns the data bytes as a SysexData object.

    Raises TypeError if value is not iterable.
    Raises TypeError if one of the bytes is not an integer.
    Raises ValueError if one of the bytes is out of range 0..127.
    """
    # Make the sequence immutable.
    data_bytes = SysexData(data_bytes)

    for byte in data_bytes:
        check_databyte(byte)

    return data_bytes


def check_frame_type(value):
    """Check type and value SMPTE quarter frame type.

    Raises TypeError if the value is not an integer.
    Raises ValueError if the value is out of range.
    """
    if not isinstance(value, int):
        raise TypeError('frame_type must be an integer')
    elif not 0 <= value <= 7:
        raise ValueError('frame_type must be in range 0..7')


def check_frame_value(value):
    """Check type and value of SMPTE quarter frame value.

    Raises TypeError if the value is not an integer.
    Raises ValueError if the value is out of range.
    """
    if not isinstance(value, int):
        raise TypeError('frame_value must be an integer')
    elif not 0 <= value <= 15:
        raise ValueError('frame_value must be in range 0..15')


def check_databyte(value):
    """Raise exception of byte has wrong type or is out of range

    Raises TypeError if the byte is not an integer, and ValueError if
    it is out of range. Data bytes are 7 bit, so the valid range is
    0..127.
    """
    if not isinstance(value, int):
        raise TypeError('data byte must be an integer')
    elif not 0 <= value <= 127:
        raise ValueError('data byte must be in range 0..127')


def encode_channel(channel):
    """Convert channel into a list of bytes. Return an empty list of
    bytes, since channel is already masked into status byte.
    """
    return []


def encode_data(data):
    """Encode sysex data as a list of bytes. A sysex end byte (0xf7)
    is appended.
    """
    return list(data) + [0xf7]


def encode_pitch(pitch):
    """Encode pitchwheel pitch as a list of bytes."""
    pitch -= MIN_PITCHWHEEL
    return [pitch & 0x7f, pitch >> 7]


def encode_pos(pos):
    """Encode song position as a list of bytes."""
    return [pos & 0x7f, pos >> 7]


class BaseMessage(object):
    """Base class for MIDI messages.

    Can be subclassed to create meta messages, for example.
    """

    def copy(self, **overrides):
        """Return a copy of the message.

        Attributes will be overriden by the passed keyword arguments.
        Only message specific attributes can be overridden. The message
        type can not be changed.

        Example:

            a = Message('note_on')
            b = a.copy(velocity=32)
        """

        # Make an exact copy of this object.
        klass = self.__class__
        message = klass.__new__(klass)
        message.__dict__.update(self.__dict__)

        for name, value in overrides.items():
            try:
                # setattr() is responsible for checking the
                # name and type of the atrribute.
                setattr(message, name, value)
            except AttributeError as err:
                raise ValueError(*err.args)

        return message

    def bytes(self):
        raise ValueError('bytes() is not implemented in this class')

    def bin(self):
        """Encode message and return as a bytearray.

        This can be used to write the message to a file.
        """
        return bytearray(self.bytes())

    def hex(self, sep=' '):
        """Encode message and return as a string of hex numbers,

        Each number is separated by the string sep.
        """
        return sep.join('{:02X}'.format(byte) for byte in self.bytes())

    def __eq__(self, other):
        """Compare message to another for equality.

        Key for comparison: (msg.type, msg.channel, msg.note, msg.velocity).
        """
        if not isinstance(other, BaseMessage):
            raise TypeError('comparison between message and another type')

        return self.bytes() == other.bytes()


class SysexData(tuple):
    """Special kind of tuple accepts and converts any sequence in +=."""

    def __iadd__(self, other):
        return SysexData(self + check_data(other))


class Message(BaseMessage):
    """
    MIDI message class.
    """

    # Quick lookup of specs by name or status_byte.
    _spec_lookup = build_spec_lookup(get_message_specs())

    # This is needed for __init__() so it doesn't accept status bytes.
    _type_lookup = {name: value for name, value in _spec_lookup.items() \
                    if not isinstance(name, int)}

    @staticmethod
    def get_spec(type_or_status_byte):
        """Get message specification from status byte or message type name.

        For use in writing parsers.
        """
        try:
            return Message._spec_lookup[type_or_status_byte]
        except KeyError:
            raise LookupError('unknown type or status byte')

    def __init__(self, type, **arguments):
        """Create a new message.

        The first argument is typically the type of message to create,
        for example 'note_on'.
        """
        try:
            spec = self._type_lookup[type]
        except KeyError:
            raise ValueError('invalid message type {!r}'.format(type))

        self.__dict__['type'] = type
        self.__dict__['_spec'] = spec

        # Set default values.
        for name in spec.arguments:
            if name == 'velocity':
                self.__dict__['velocity'] = 0x40
            elif name == 'data':
                self.__dict__['data'] = SysexData()
            else:
                self.__dict__[name] = 0
        self.__dict__['time'] = 0

        # Override defaults.
        for name, value in arguments.items():
            try:
                setattr(self, name, value)
            except AttributeError as err:
                raise ValueError(*err.args)

    def __setattr__(self, name, value):
        if name in self._spec.settable_attributes:
            try:
                if name == 'data':
                    value = check_data(value)
                else:
                    globals()['check_{}'.format(name)](value)
            except KeyError:
                check_databyte(value)

            self.__dict__[name] = value
        elif name in self.__dict__:
            raise AttributeError('{} attribute is read only'.format(name))
        else:
            raise AttributeError(
                '{} message has no attribute {}'.format(self.type, name))

    def __delattr__(self, name):
        raise AttributeError('attribute can not be deleted')

    def bytes(self):
        """Encode message and return as a list of integers."""

        status_byte = self._spec.status_byte
        if status_byte < 0xf0:
            # Add channel (lower 4 bits) to status byte.
            # Those bits in spec.status_byte are always 0.
            status_byte |= self.channel

        message_bytes = [status_byte]

        if self.type == 'quarter_frame':
            message_bytes.append(self.frame_type << 4 | self.frame_value)
        else:
            for name in self._spec.arguments:
                value = getattr(self, name)
                try:
                    encode = globals()['encode_{}'.format(name)]
                    message_bytes.extend(encode(value))
                except KeyError:
                    message_bytes.append(value)

        return message_bytes

    def __repr__(self):
        parts = []

        for name in self._spec.arguments + ('time',):
            parts.append('{}={!r}'.format(name, getattr(self, name)))

        return '<message {} {}>'.format(self.type, ' '.join(parts))

    def __str__(self):
        return format_as_string(self)

    def __len__(self):
        if self.type == 'sysex':
            return len(self.data) + 2
        else:
            return self._spec.length


def parse_time(text):
    if text.endswith('L'):
        raise ValueError('L is not allowed in time')

    if PY2:
        converters = [int, long, float]
    else:
        converters = [int, float]

    for convert in converters:
        try:
            return convert(text)
        except ValueError:
            pass

    raise ValueError('invalid format for time')


def parse_string(text):
    """Parse a string of text and return a message.

    The string can span multiple lines, but must contain
    one full message.

    Raises ValueError if the string could not be parsed.
    """
    words = text.split()

    type_name = words[0]
    arguments = words[1:]

    names_seen = set()

    kwargs = {}

    for argument in arguments:
        try:
            name, value = argument.split('=')
        except ValueError:
            raise ValueError('missing or extraneous equals sign')

        if name in names_seen:
            raise ValueError('argument passed more than once')
        names_seen.add(name)

        if name == 'data':
            if not value.startswith('(') and value.endswith(')'):
                raise ValueError('missing parentheses in data message')

            try:
                data_bytes = [int(byte) for byte in value[1:-1].split(',')]
            except ValueError:
                raise ValueError('unable to parse data bytes')
            kwargs['data'] = data_bytes
        elif name == 'time':
            try:
                time = parse_time(value)
            except ValueError:
                raise ValueError('invalid value for time')
            try:
                kwargs['time'] = time
            except AttributeError as err:
                raise ValueError(err.message)
            except TypeError as err:
                raise ValueError(err.message)
        else:
            try:
                kwargs[name] = int(value)
            except AttributeError as exception:
                raise ValueError(*exception.args)
            except ValueError:
                raise ValueError('{!r} is not an integer'.format(value))

    return Message(type_name, **kwargs)


def parse_string_stream(stream):
    """Parse a stram of messages and yield (message, error_message)

    stream can be any iterable that generates text strings, where each
    string is a string encoded message.

    If a string can be parsed, (message, None) is returned. If it
    can't be parsed (None, error_message) is returned. The error
    message containes the line number where the error occurred.
    """
    line_number = 1
    for line in stream:
        try:
            line = line.split('#')[0].strip()
            if line:
                yield parse_string(line), None
        except ValueError as exception:
            error_message = 'line {line_number}: {message}'.format(
                line_number=line_number,
                message=exception.args[0])
            yield None, error_message
        line_number += 1


def format_as_string(message, include_time=True):
    """Format a message and return as a string.

    This is equivalent to str(message).

    To leave out the time attribute, pass include_time=False.
    """
    if not isinstance(message, Message):
        raise ValueError('message must be a mido.Message object')

    words = []
    words.append(message.type)

    names = message._spec.arguments
    if include_time:
        names += ('time',)

    for name in names:
        value = getattr(message, name)
        if name == 'data':
            value = '({})'.format(','.join(str(byte) for byte in value))
        elif name == 'time':
            # Python 2 formats longs as '983989385L'. This is not allowed.
            value = str(value)
            value = value.replace('L', '')
        words.append('{}={}'.format(name, value))

    return ' '.join(words)




"""

MIDI meta messages

"""

_charset = 'latin1'


def reverse_table(table):
    """Return value: key for dictionary."""
    return {value: key for (key, value) in table.items()}


_key_signature_decode = {
    (-7, 0): 'Cb',
    (-6, 0): 'Gb',
    (-5, 0): 'Db',
    (-4, 0): 'Ab',
    (-3, 0): 'Eb',
    (-2, 0): 'Bb',
    (-1, 0): 'F',
    (0, 0): 'C',
    (1, 0): 'G',
    (2, 0): 'D',
    (3, 0): 'A',
    (4, 0): 'E',
    (5, 0): 'B',
    (6, 0): 'F#',
    (7, 0): 'C#',
    (-7, 1): 'Abm',
    (-6, 1): 'Ebm',
    (-5, 1): 'Bbm',
    (-4, 1): 'Fm',
    (-3, 1): 'Cm',
    (-2, 1): 'Gm',
    (-1, 1): 'Dm',
    (0, 1): 'Am',
    (1, 1): 'Em',
    (2, 1): 'Bm',
    (3, 1): 'F#m',
    (4, 1): 'C#m',
    (5, 1): 'G#m',
    (6, 1): 'D#m',
    (7, 1): 'A#m',
}
_key_signature_encode = reverse_table(_key_signature_decode)

_smpte_framerate_decode = {
    0: 24,
    1: 25,
    2: 29.97,
    3: 30,
}
_smpte_framerate_encode = reverse_table(_smpte_framerate_decode)


def signed(to_type, n):
    formats = {
        'byte': 'Bb',
        'short': 'Hh',
        'long': 'Ll',
        'ubyte': 'bB',
        'ushort': 'hH',
        'ulong': 'lL',
    }

    try:
        pack_format, unpack_format = formats[to_type]
    except KeyError:
        raise ValueError('invalid integer type {}'.format(to_type))

    try:
        packed = struct.pack(pack_format, n)
        return struct.unpack(unpack_format, packed)[0]
    except struct.error as err:
        raise ValueError(*err.args)


def unsigned(to_type, n):
    return signed('u{}'.format(to_type), n)


def encode_variable_int(value):
    """Encode variable length integer.

    Returns the integer as a list of bytes,
    where the last byte is < 128.

    This is used for delta times and meta message payload
    length.
    """
    if not isinstance(value, int) or value < 0:
        raise ValueError('variable int must be a positive integer')

    bytes = []
    while value:
        bytes.append(value & 0x7f)
        value >>= 7

    if bytes:
        bytes.reverse()

        # Set high bit in every byte but the last.
        for i in range(len(bytes) - 1):
            bytes[i] |= 0x80
        return bytes
    else:
        return [0]


def encode_string(string):
    return list(bytearray(string.encode(_charset)))


def decode_string(data):
    return bytearray(data).decode(_charset)


def bpm2tempo(bpm):
    """Convert beats per minute to MIDI file tempo.

    Returns microseconds per beat as an integer::

        240 => 250000
        120 => 500000
        60 => 1000000
    """
    # One minute is 60 million microseconds.
    return int(round((60 * 1000000) / bpm))


def tempo2bpm(tempo):
    """Convert MIDI file tempo to BPM.

    Returns BPM as an integer or float::

        250000 => 240
        500000 => 120
        1000000 => 60
    """
    # One minute is 60 million microseconds.
    return (60 * 1000000) / tempo


@contextmanager
def meta_charset(tmp_charset):
    global _charset
    old = _charset
    _charset = tmp_charset
    yield
    _charset = old


def check_int(value, low, high):
    if not isinstance(value, int):
        raise TypeError('attribute must be an integer')
    elif not low <= value <= high:
        raise ValueError('attribute must be in range {}..{}'.format(low, high))


if PY2:
    def check_str(value):
        if not isinstance(value, basestring):
            raise TypeError('attribute must be unicode or string')
else:
    def check_str(value):
        if not isinstance(value, str):
            raise TypeError('attribute must a string')


class MetaSpec(object):
    def check(self, name, value):
        pass


class MetaSpec_sequence_number(MetaSpec):
    type_byte = 0x00
    attributes = ['number']
    defaults = [0]

    def decode(self, message, data):
        message.number = (data[0] << 8) | data[1]

    def encode(self, message):
        return [message.number >> 8, message.number & 0xff]

    def check(self, name, value):
        check_int(value, 0, 255)


class MetaSpec_text(MetaSpec):
    type_byte = 0x01
    attributes = ['text']
    defaults = ['']

    def decode(self, message, data):
        message.text = decode_string(data)

    def encode(self, message):
        return encode_string(message.text)

    def check(self, name, value):
        check_str(value)


class MetaSpec_copyright(MetaSpec_text):
    type_byte = 0x02


class MetaSpec_track_name(MetaSpec_text):
    type_byte = 0x03
    attributes = ['name']
    defaults = ['']

    def decode(self, message, data):
        message.name = decode_string(data)

    def encode(self, message):
        return encode_string(message.name)


class MetaSpec_instrument_name(MetaSpec_track_name):
    type_byte = 0x04


class MetaSpec_lyrics(MetaSpec_text):
    type_byte = 0x05


class MetaSpec_marker(MetaSpec_text):
    type_byte = 0x06


class MetaSpec_cue_marker(MetaSpec_text):
    type_byte = 0x07


class MetaSpec_device_name(MetaSpec_track_name):
    type_byte = 0x09


class MetaSpec_channel_prefix(MetaSpec):
    type_byte = 0x20
    attributes = ['channel']
    defaults = [0]

    def decode(self, message, data):
        message.channel = data[0]

    def encode(self, message):
        return [message.channel]

    def check(self, name, value):
        check_int(value, 0, 0xff)


class MetaSpec_midi_port(MetaSpec):
    type_byte = 0x21
    attributes = ['port']
    defaults = [0]

    def decode(self, message, data):
        message.port = data[0]

    def encode(self, message):
        return [message.port]

    def check(self, name, value):
        check_int(value, 0, 255)


class MetaSpec_end_of_track(MetaSpec):
    type_byte = 0x2f
    attributes = []
    defaults = []

    def decode(self, message, data):
        pass

    def encode(self, message):
        return []


class MetaSpec_set_tempo(MetaSpec):
    type_byte = 0x51
    attributes = ['tempo']
    defaults = [500000]

    def decode(self, message, data):
        message.tempo = (data[0] << 16) | (data[1] << 8) | (data[2])

    def encode(self, message):
        tempo = message.tempo
        return [tempo >> 16, tempo >> 8 & 0xff, tempo & 0xff]

    def check(self, name, value):
        check_int(value, 0, 0xffffff)


class MetaSpec_smpte_offset(MetaSpec):
    type_byte = 0x54
    attributes = ['frame_rate',
                  'hours',
                  'minutes',
                  'seconds',
                  'frames',
                  'sub_frames'
                  ]
    # Todo: What are some good defaults?
    defaults = [24, 0, 0, 0, 0, 0]

    def decode(self, message, data):
        message.frame_rate = _smpte_framerate_decode[(data[0] >> 6)]
        message.hours = (data[0] & 0x3f)
        message.minutes = data[1]
        message.seconds = data[2]
        message.frames = data[3]
        message.sub_frames = data[4]

    def encode(self, message):
        frame_rate_lookup = _smpte_framerate_encode[message.frame_rate] << 6
        return [frame_rate_lookup | message.hours,
                message.minutes,
                message.seconds,
                message.frames,
                message.sub_frames]

    def check(self, name, value):
        if name == 'frame_rate':
            if value not in _smpte_framerate_encode:
                valid = ', '.join(sorted(_smpte_framerate_encode.keys()))
                raise ValueError('frame_rate must be one of {}'.format(valid))
        elif name == 'hours':
            check_int(value, 0, 255)
        elif name in ['minutes', 'seconds']:
            check_int(value, 0, 59)
        elif name == 'frames':
            check_int(value, 0, 255)
        elif name == 'sub_frames':
            check_int(value, 0, 99)


class MetaSpec_time_signature(MetaSpec):
    type_byte = 0x58
    # Todo: these need more sensible names.
    attributes = ['numerator',
                  'denominator',
                  'clocks_per_click',
                  'notated_32nd_notes_per_beat']
    defaults = [4, 2, 24, 8]

    def decode(self, message, data):
        message.numerator = data[0]
        message.denominator = 2 ** data[1]
        message.clocks_per_click = data[2]
        message.notated_32nd_notes_per_beat = data[3]

    def encode(self, message):
        return [
            message.numerator,
            int(math.log(message.denominator, 2)),
            message.clocks_per_click,
            message.notated_32nd_notes_per_beat,
        ]

    def check(self, name, value):
        if name == 'denominator':
            # This allows for the ridiculous time signature of
            # 4/57896044618658097711785492504343953926634...
            #   992332820282019728792003956564819968
            check_int(value, 1, 2 ** 255)
            encoded = math.log(value, 2)
            encoded_int = int(encoded)
            if encoded != encoded_int:
                raise ValueError('denominator must be a power of 2')
        else:
            check_int(value, 0, 255)


class MetaSpec_key_signature(MetaSpec):
    type_byte = 0x59
    attributes = ['key']
    defaults = ['C']

    def decode(self, message, data):
        key = signed('byte', data[0])
        mode = data[1]
        message.key = _key_signature_decode[(key, mode)]

    def encode(self, message):
        key, mode = _key_signature_encode[message.key]
        return [unsigned('byte', key), mode]

    def check(self, name, value):
        if not value in _key_signature_encode:
            raise ValueError('invalid key {!r}'.format(value))


class MetaSpec_sequencer_specific(MetaSpec):
    type_byte = 0x7f
    attributes = ['data']
    defaults = [[]]

    def decode(self, message, data):
        message.data = tuple(data)

    def encode(self, message):
        return list(message.data)


_specs = {}


def add_meta_spec(klass):
    spec = klass()
    if not hasattr(spec, 'type'):
        name = klass.__name__.replace('MetaSpec_', '')
        spec.type = name
    # This is used by copy().
    spec.settable_attributes = set(spec.attributes) | {'time'}
    _specs[spec.type_byte] = spec
    _specs[spec.type] = spec


def _add_builtin_meta_specs():
    for name in globals():
        if name.startswith('MetaSpec_'):
            add_meta_spec(globals()[name])


_add_builtin_meta_specs()


def _build_meta_message(type_, data):
    # Todo: handle unknown type.
    try:
        spec = _specs[type_]
    except KeyError:
        return UnknownMetaMessage(type_, data)

    message = MetaMessage(spec)
    spec.decode(message, data)
    return message


class MetaMessage(BaseMessage):
    def __init__(self, type_, **kwargs):
        # Todo: allow type_ to be a type byte?
        # Todo: handle unknown type.
        if isinstance(type_, MetaSpec):
            self.__dict__['_spec'] = type_
        else:
            self.__dict__['_spec'] = _specs[type_]

        self.__dict__['type'] = self._spec.type

        for name in kwargs:
            if name == 'time':
                continue  # Time is always allowed.

            if name not in self._spec.settable_attributes:
                raise ValueError(
                    '{} is not a valid argument for this message type'.format(
                        name))

        for name, value in zip(self._spec.attributes, self._spec.defaults):
            self.__dict__[name] = value
        self.__dict__['time'] = 0

        for name, value in kwargs.items():
            setattr(self, name, value)

    def __setattr__(self, name, value):
        if name in self._spec.settable_attributes:
            if name == 'time':
                check_time(value)
            else:
                self._spec.check(name, value)
            self.__dict__[name] = value
        elif name in self.__dict__:
            raise AttributeError('{} attribute is read only'.format(name))
        else:
            raise AttributeError(
                '{} message has no attribute {}'.format(self.type, name))

    def bytes(self):
        data = self._spec.encode(self)
        return ([0xff, self._spec.type_byte]
                + encode_variable_int(len(data))
                + data)

    def __repr__(self):
        attributes = []
        for name in self._spec.attributes:
            attributes.append('{}={!r}'.format(name, getattr(self, name)))
        attributes = ' '.join(attributes)
        if attributes:
            attributes = (' {}'.format(attributes))

        return '<meta message {}{} time={}>'.format(self.type,
                                                    attributes, self.time)


class UnknownMetaMessage(MetaMessage):
    def __init__(self, type_byte, data=None, time=0):
        if data is None:
            data = []

        self.type = 'unknown_meta'
        self._type_byte = type_byte
        self._data = data
        self.time = time

    def __repr__(self):
        return ('<unknown meta message'
                ' _type_byte=0x{:02x} _data={!r} time={}>').format(
            self._type_byte,
            self._data,
            self.time)

    # Override all checking.
    def __setattr__(self, name, value):
        self.__dict__[name] = value

    def bytes(self):
        return ([0xff, self._type_byte]
                + encode_variable_int(len(self._data))
                + self._data)







DEFAULT_TEMPO = 500000
DEFAULT_TICKS_PER_BEAT = 480

class ByteReader(object):
    """
    Reads bytes from a file.
    """

    def __init__(self, filename):
        self._buffer = list(bytearray(open(filename, 'rb').read()))
        self.pos = 0
        self._eof = EOFError('unexpected end of file')

    def read_byte(self):
        """Read one byte."""
        try:
            byte = self._buffer[self.pos]
            self.pos += 1
            return byte
        except IndexError:
            raise self._eof

    def peek_byte(self):
        """Return the next byte in the file.

        This can be used for look-ahead."""
        try:
            return self._buffer[self.pos]
        except IndexError:
            raise self._eof

    def peek_list(self, n):
        """Return a list of the next n bytes."""
        return self._buffer[self.pos:self.pos + n]

    def read_short(self):
        """Read short (2 bytes little endian)."""
        a, b = self.read_list(2)
        return a << 8 | b

    def read_long(self):
        """Read long (4 bytes little endian)."""
        a, b, c, d = self.read_list(4)
        return a << 24 | b << 16 | c << 8 | d

    def read_list(self, n):
        """Read n bytes and return as a list."""
        i = self.pos
        ret = self._buffer[i:i + n]
        if len(ret) < n:
            raise self._eof

        self.pos += n
        return ret

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        return False

class MidiTrack(list):
    def __init__(self):
        list.__init__([])

    @property
    def name(self):
        """Name of the track.

        This will return the name from the first track_name meta
        message in the track, or '' if there is no such message.

        Setting this property will update the name field of the first
        track_name message in the track. If no such message is found,
        one will be added to the beginning of the track with a delta
        time of 0."""
        for message in self:
            if message.type == 'track_name':
                return message.name
        else:
            return u''

    @name.setter
    def name(self, name):
        # Find the first track_name message and modify it.
        for message in self:
            if message.type == 'track_name':
                message.name = name
                return
        else:
            # No track name found, add one.
            self.insert(0, MetaMessage('track_name', name=name, time=0))

    def __repr__(self):
        return '<midi track {!r} {} messages>'.format(self.name, len(self))

class MidiFile:
    def __init__(self, filename=None, type=1,
                 ticks_per_beat=DEFAULT_TICKS_PER_BEAT,
                 charset='latin1'):
        self.filename = filename
        self.tracks = []
        self.charset = charset

        if filename is None:
            if type not in range(3):
                raise ValueError(
                    'invalid format {} (must be 0, 1 or 2)'.format(format))
            self.type = type
            self.ticks_per_beat = ticks_per_beat
        else:
            self._load()

    def _load(self):
        with ByteReader(self.filename) as self._file, \
                meta_charset(self.charset):
            # Read header (16 bytes)
            magic = self._file.peek_list(4)
            if not bytearray(magic) == bytearray(b'MThd'):
                raise IOError('MThd not found. Probably not a MIDI file')
            self._file.read_list(4)  # Skip MThd

            # This is always 6 for any file created under the MIDI 1.0
            # specification, but this allows for future expansion.
            header_size = self._file.read_long()

            self.type = self._file.read_short()
            number_of_tracks = self._file.read_short()
            self.ticks_per_beat = self._file.read_short()

            # Skip the rest of the header.
            for _ in range(header_size - 6):
                self._file.read_byte()

            for i in range(number_of_tracks):
                self.tracks.append(self._read_track())
                # Todo: used to ignore EOFError. I hope things still work.

    def _read_variable_int(self):
        delta = 0

        while True:
            byte = self._file.read_byte()
            delta = (delta << 7) | (byte & 0x7f)
            if byte < 0x80:
                return delta

    def _build_message(self, spec, bytes, time=0):
        """Build message from bytes.

        bytes is a full list of bytes for the message
        including the status byte. For sysex messages,
        the end byte is not included.
        Examples:

            build_message(spec, [0x80, 20, 100])
            build_message(spec, [0xf0, 1, 2, 3])

        No type or value checking is done, so you need to do that before
        you call this function. (This includes time.) 0xf7 is not allowed
        as status byte.
        """
        # This could be written in a more general way, but most messages
        # are note_on or note_off so doing it this way is faster.
        if spec.type in ['note_on', 'note_off']:
            attributes = {
                    'channel': bytes[0] & 0x0f,
                    'note': bytes[1],
                    'velocity': bytes[2],
                    }

        elif spec.type == 'control_change':
            attributes = {
                    'channel': bytes[0] & 0x0f,
                    'control': bytes[1],
                    'value': bytes[2],
                    }

        elif spec.status_byte < 0xf0:
            # Channel message. The most common type.
            if spec.type == 'pitchwheel':
                pitch = bytes[1] | ((bytes[2] << 7) + MIN_PITCHWHEEL)
                attributes = {'pitch': pitch}
            else:
                attributes = dict(zip(spec.arguments, bytes))
            # Replace status_bytes sneakily with channel.
            attributes['channel'] = bytes[0] & 0x0f

        elif spec.type == 'sysex':
            attributes = {'data': tuple(bytes[1:])}

        elif spec.type == 'songpos':
            pos = bytes[1] | (bytes[2] << 7)
            attributes = {'pos': pos}

        elif spec.type == 'quarter_frame':
            attributes = {'frame_type': bytes[1] >> 4,
                         'frame_value' : bytes[1] & 15}

        else:
            attributes = dict(zip(spec.arguments, bytes[1:]))

        # Message.__new__() is used as an optimization to
        # get around argument checking. We already know that
        # the values are valid.
        message = Message.__new__(Message)
        message.__dict__.update(attributes)
        message.__dict__.update({
                'type': spec.type,
                '_spec': spec,
                'time': time,
                })
        return message

    def _read_message(self, status_byte):
        try:
            spec = Message.get_spec(status_byte)
        except LookupError:
            raise IOError('undefined status byte 0x{:02x}'.format(status_byte))
        data_bytes = self._file.read_list(spec.length - 1)
        for byte in data_bytes:
            if byte > 127:
                raise IOError('data byte must be in range 0..127')
        return self._build_message(spec, [status_byte] + data_bytes)

    def _read_meta_message(self):
        type = self._file.read_byte()
        length = self._read_variable_int()
        data = self._file.read_list(length)
        return _build_meta_message(type, data)

    def _read_sysex(self):
        length = self._read_variable_int()
        data = self._file.read_list(length)

        # Strip start and end bytes.
        if data and data[0] == 0xf0:
            data = data[1:]
        if data and data[-1] == 0xf7:
            data = data[:-1]

        message = Message('sysex', data=data)
        return message

    def _read_track(self):
        track = MidiTrack()

        magic = self._file.peek_list(4)
        if bytearray(magic) == bytearray(b'MTrk'):
            self._file.read_list(4)  # Skip 'MTrk'
            length = self._file.read_long()
        else:
            # Todo: some files don't have track headers?
            # These end with end_of_track or end of file,
            # so we set length to infinite.
            # raise IOError("track doesn't start with 'MTrk'")
            length = float('inf')

        start = self._file.pos
        last_status = None

        while True:
            # End of track reached.
            if self._file.pos - start == length:
                break

            delta = self._read_variable_int()

            # Todo: not all messages have running status
            peek_status = self._file.peek_byte()
            if peek_status < 0x80:
                if last_status is None:
                    raise IOError('running status without last_status')
                status_byte = last_status
            else:
                status_byte = self._file.read_byte()
                if status_byte != 0xff:
                    # Meta messages don't set running status.
                    last_status = status_byte

            if status_byte == 0xff:
                message = self._read_meta_message()
            elif status_byte in [0xf0, 0xf7]:
                # Todo: I'm not quite clear on the difference between
                # f0 and f7 events.
                message = self._read_sysex()
            else:
                message = self._read_message(status_byte)

            message.time = delta
            track.append(message)

            if message.type == 'end_of_track':
                break

        return track

    @property
    # СН - использует
    # атрибуты:
    #       type (проверка на неравенство 2)
    #       message (для доступа к message.time)
    # методы:
    #       __iter__(self) неявным образом, для выборки в цикле

    def length(self):
        """Playback time in seconds.

        This will be computed by going through every message in every
        track and adding up delta times.
        """
        if self.type == 2:
            raise ValueError('impossible to compute length'
                             ' for type 2 (asynchronous) file')

        return sum(message.time for message in self)

    def _get_seconds_per_tick(self, tempo):
        # Tempo is given in microseconds per beat (default 500000).
        # At this tempo there are (500000 / 1000000) == 0.5 seconds
        # per beat. At the default resolution of 480 ticks per beat
        # this is:
        #
        #    (500000 / 1000000) / 480 == 0.5 / 480 == 0.0010417
        #
        return (tempo / 1000000.0) / self.ticks_per_beat

    def _merged_track(self):
        """
        Returns a MidiTrack object with all messages from all tracks.

        The messages are returned in playback order with delta times
        as if they were all in one track.
        """
        max_time = 0
        messages = MidiTrack()
        for track in self.tracks:
            now = 0
            for message in track:
                now += message.time
                if message.type not in ('track_name', 'end_of_track'):
                    messages.append(message.copy(time=now))
                if message.type == 'end_of_track':
                    break
            max_time = max(max_time, now)

        messages.sort(key=lambda x: x.time)
        messages.append(MetaMessage('end_of_track', time=max_time))

        # Convert absolute time back to delta time.
        last_time = 0
        for message in messages:
            message.time -= last_time
            last_time += message.time

        return messages

    def __iter__(self):
        # The tracks of type 2 files are not in sync, so they can
        # not be played back like this.
        if self.type == 2:
            raise TypeError("can't merge tracks in type 2 (asynchronous) file")

        seconds_per_tick = self._get_seconds_per_tick(DEFAULT_TEMPO)

        for message in self._merged_track():
            # Convert message time from absolute time
            # in ticks to relative time in seconds.
            if message.time > 0:
                message.time *= seconds_per_tick
            else:
                message.time = 0

            yield message

            if message.type == 'set_tempo':
                seconds_per_tick = self._get_seconds_per_tick(message.tempo)

    def __repr__(self):
        return '<midi file {!r} type {}, {} tracks, {} messages>'.format(
            self.filename, self.type, len(self.tracks),
            sum([len(track) for track in self.tracks]))

