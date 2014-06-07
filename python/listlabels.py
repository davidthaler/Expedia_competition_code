datapath = 'data/'
test_label_file = 'test.labels.csv'
train_label_file = 'train.labels.csv'
outfile = 'labels.list.csv'

f_test = open(datapath + test_label_file)
test_labels = f_test.readline()
f_test.close()
f_train = open(datapath + train_label_file)
train_labels = f_train.readline()
f_train.close()

f_out = open(datapath + outfile, 'w')
print >> f_out, 'Training Labels:'
idx = 1
for wd in train_labels.split(','):
  print >> f_out, idx, ') ', wd
  idx = idx + 1

idx = 1  
print >> f_out, '\n\nTest Labels:'
for wd in test_labels.split(','):
  print >> f_out, idx, ') ', wd
  idx = idx + 1
f_out.close()