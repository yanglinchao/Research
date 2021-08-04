# -*- coding: utf-8 -*-
"""
Created on Mon Jul 12 00:04:19 2021

@author: ylc
"""

import numpy as np
import pandas as pd
import jieba
from gensim import corpora, models
from gensim.models.doc2vec import Doc2Vec, TaggedDocument

# 导入数据
data_tf = pd.read_csv('原始数据.csv',
                      names=['深层现象', '故障现象', '故障分析', '处理方式'],
                      encoding='utf-8')
data_tf.columns = ['deepph', 'ph', 'analyze', 'handle']
data_tf = data_tf[1:]
data_tf.index = pd.Series(range(1, 878))

stopwords = stopwordslist(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/stopwords/bd_hit_scu_stopwords.txt")
# 对故障现象(ph)进行分词
data_cut_ph = data_tf.ph
list_result_cut_ph = simpleWordCut(pd.Series.tolist(data_cut_ph.astype(str)), stopwords, cut_all=False, HMM=False)

# 提取TF-IDF特征向量
list_tfidf = tfidf_Gensim(list_result_cut_ph)

