import sys

folder = 'Parsed/'

def main(argv):
  if not len(argv) == 1: return
  f_read = open(argv[0], 'r')

  filename = argv[0].split('.')
  scene_num = 0
  f_write = open(filename[0] + '_' + str(scene_num) + '.txt', 'w')

  for line in f_read:
    # if reach ext/int, finish writing and open new file
    if (line.strip().lower().startswith('ext.') or
        line.strip().lower().startswith('int.') or
        line.strip().lower().startswith('internal') or
        line.strip().lower().startswith('external') or
        line.strip().lower().startswith('scene:')):
      f_write.close()
      scene_num += 1
      f_write = open(folder + filename[0] + '_' + str(scene_num) + '.txt', 'w')
    # if reach the end, stop
    if line.strip().lower() == 'the end':
      break
    # write the line
    f_write.write(line.lower())

  f_write.close()
  f_read.close()

if __name__ == '__main__':
  main(sys.argv[1:])
