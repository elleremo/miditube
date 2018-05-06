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


# Класс файловой сущности, файлы и папки
# class Fileobj:
#    def __init__(self):
#        print("from file obj")


class Folder:

    def __init__(self, name, parentfolder):
        self.name = name
        self.parentfolder = parentfolder
        self.path = parentfolder + '/' + name
        self.files = pydir.get_files(self.path)

   # def getfilesExt (self):


class MidFile:
    duration = 0.0
    artist = ""

    def __init__(self, name, parentfolder):
        self.name = name
        self.path = parentfolder + '/' + name
        self.duration = MidiFile(self.name).length

    def setArtist(self, folder):
        self.artist = folder.name


def getListFolders(folder):
    if not pydir.is_empty_dir(folder):
        subdirsname = pydir.get_subdirs(folder)
        subdirs = []
        for dir in subdirsname:
            temp = Folder(dir, folder)
            subdirs.append(temp)

    return subdirs


def getListFiles(folder, ext):
    if not pydir.is_empty_dir(folder):
        files = []
        for file in pydir.get_files(folder, ext):
            temp = MidFile(file, folder)
            print(file, folder)
            files.append(temp)

    return files


z = Folder()