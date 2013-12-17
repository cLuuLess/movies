import nltk
import os
import string
import sys
from collections import Counter

folder = 'Parsed/'
totalwordcounts = Counter()

def split_files(filename):
  f_read = open(filename, 'r')

  fname_split = filename.split('.')
  print fname_split
  scene_num = 0
  f_write = open(fname_split[0] + '_' + str(scene_num) + '.txt', 'w')

  for line in f_read:
    # if reach ext/int, finish writing and open new file
    line = line.lower()
    if (line.strip().startswith('ext.') or
        line.strip().startswith('int.') or
        line.strip().startswith('internal') or
        line.strip().startswith('external') or
        line.strip().startswith('scene:')):
      f_write.close()
      scene_num += 1
      f_write = open(folder + fname_split[0] + '_' + str(scene_num).rjust(3, '0') + '.txt', 'w')
      f_write.write(line)
      continue
    # if reach the end, stop
    if line.strip() == 'the end':
      break
    # write the line
    totalwordcounts.update(Counter(nltk.word_tokenize(line.translate(None, string.punctuation))))
    f_write.write(line)

  f_write.close()
  f_read.close()
  return totalwordcounts

def write_xtab():
  xtab = open(folder + 'xtab.txt', 'w')
  # write allwords to xtab
  for w in sorted(totalwordcounts, key=totalwordcounts.get, reverse=True):
    xtab.write(w + ' ')

  # for each txt file, write the vector
  for item in sorted(os.listdir(folder)): 
    if os.path.isfile(os.path.join(folder, item)): 
      fileName, fileExtension = os.path.splitext(item)
      if fileExtension.lower() == '.txt' and not fileName == 'xtab':
        xtab.write('\n' + item + ' ')
        cur_dict = dict.fromkeys(totalwordcounts.keys())
        cur_read = open(os.path.join(folder, item), 'r')
        cur_read.readline()
        
        cur_words = Counter(nltk.word_tokenize(cur_read.read().translate(None, string.punctuation)))
          
        for w in sorted(totalwordcounts, key=totalwordcounts.get, reverse=True):
          xtab.write(str(cur_words[w]) + ' ')
  xtab.close()

def main(argv):
  if not len(argv) == 1:
    print 'Usage: python script_parser.py [filename]'
    return
  split_files(argv[0])
  write_xtab()

if __name__ == '__main__':
  main(sys.argv[1:])
