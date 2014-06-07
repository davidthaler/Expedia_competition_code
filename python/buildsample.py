datapath = 'data/'
trainfile = 'train.csv'
testfile = 'test.csv'
train_label_file = 'train.labels.csv'
train_sample_file = 'train.sample.csv'
test_label_file = 'test.labels.csv'
test_sample_file = 'test.sample.csv'
n_lines = 500

f_train = open(datapath + trainfile)
f_train_labels = open(datapath + train_label_file, 'w')
f_train_sample = open(datapath + train_sample_file, 'w')
f_train_labels.write(f_train.readline())
for k in range(n_lines):
  f_train_sample.write(f_train.readline())
f_train_sample.close()
f_train_labels.close()
f_train.close()

f_test = open(datapath + testfile)
f_test_labels = open(datapath + test_label_file, 'w')
f_test_sample = open(datapath + test_sample_file, 'w')
f_test_labels.write(f_test.readline())
for k in range(n_lines):
  f_test_sample.write(f_test.readline())
f_test_sample.close()
f_test_labels.close()
f_test.close()
