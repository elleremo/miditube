import pydir
import os
from mdfile import MidiFile

# os.listdir(path=".") - список файлов и директорий в папке.
# os.chdir(path) - смена текущей директории.

main_dir = os.getcwd()  # текущая дериктория
# print("Текущая дериктория =", main_dir)

unrec_dir = "D:/Projects SSD/YouTube 2018/PyTube/Unrecorded"
rec_dir = "D:/Projects SSD/YouTube 2018/PyTube/Recorded"
midi_dir = "D:/Projects SSD/YouTube 2018/PyTube/Midi"
gtp_midi = "D:/Projects SSD/YouTube 2018/PyTube/GTP_MIDI"

print(os.path.splitext(unrec_dir))