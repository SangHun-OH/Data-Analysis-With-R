library(readxl)
library(gmodels)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(doBy)
library(sampling)
library(caret)
library(rpart)
library(e1071)
library(tm)
library(SnowballC)

sms_raw<-read.csv("D:/Study/Data Science/spam.csv", stringsAsFactors = FALSE)
sms_raw$type <- factor(sms_raw$type)
# �����͸� �ҷ��� ��, ���ڸ޼����� Factor�� �޾ƿ��� �ʵ����ϰ�
# label�� �ٽ� factor�� �������ݴϴ�.
# ham�� spam�� ������ Ȯ���մϴ�.
table(sms_raw$type)

# ���۽� ��ü���� <- ����ȭ <- �ؽ�Ʈ ����
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# tm_map �Լ��� ����ؼ� �� corpus�鿡 �Լ��� ���� -> �ҹ���ȭ
sms_corpus_clean <- tm_map(sms_corpus, tolower)

# ���Ϳ� ������ ����� ��찡 ��Ȥ �ֽ��ϴ�.
# �׷��� ��쿡�� `content_transformer`�� �����մϴ�.
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# ���� ����
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# ������ 
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# stop words
stopwords()
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())

# ���¼� �м�
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)

# �߰� ���� ����
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)

# �ܾ� ��ūȭ
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm <- DocumentTermMatrix(sms_corpus,
                              control=list(
                                tolower = TRUE,
                                removeNumbers = TRUE,
                                stopwords = TRUE,
                                removePunctuation = TRUE,
                                stemming = TRUE))

# Ư������ ���ſ� �Լ�
replacePunctuation <- function(x){
  gsub(pattern = "[[:punct:]]+", " ", x)
}

# ���� ���ſ� �Լ�
replaceNumber <- function(x){
  gsub(pattern = "[[:digit:]]+", " ", x)
}

# ���� �Լ��� ������ ���� content_transformer �����ϴ� ���� ����
# �����Ϳ� ����
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(replacePunctuation))
sms_corpus_clean <- tm_map(sms_corpus_clean, content_transformer(replaceNumber))

# ������ ��ó���� ��ūȭ �� ��, �ɼ����� ����
sms_dtm<-DocumentTermMatrix(sms_corpus_clean,
                            control=list(
                              tolower = TRUE,
                              stopwords = TRUE,
                              stemming = TRUE
                            ))

# train data�� test ������ ������
sms_dtm_train <- sms_dtm[1:4167,]
sms_dtm_test <- sms_dtm[4167:5559,]

sms_train_labels <- sms_raw[1:4167,]$type
sms_test_labels <- sms_raw[4167:5559,]$type

# 5�� �̻� ���� feature ����
sms_freq_words <- findFreqTerms(sms_dtm_train,5)

# �ش� feature�� �ش�Ǵ� ���� ����
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[,sms_freq_words]

# ���������� �ٲٱ����� �Լ� ����
convert_counts<- function(x){ x <- ifelse(x>0, 'YES', "NO")}

# MARGIN=2 ���� �������� �Լ� ����
sms_train <- apply(sms_dtm_freq_train,MARGIN = 2,FUN = convert_counts)
sms_test <- apply(sms_dtm_freq_test,MARGIN = 2,FUN = convert_counts)

# ���� �� ����
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_test_labels,
           prop.t=FALSE, prop.r = FALSE,
           dnn=c('predicted','actual'))
# ���� ���� Ȯ��
sum(sms_test_pred == sms_test_labels)*100/length(sms_test_labels)
