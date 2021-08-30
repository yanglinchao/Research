# -*- coding: utf-8 -*-
"""
Created on Mon Jul 12 00:04:19 2021

@author: ylc
"""
import pandas as pd
import numpy as np

exec(open(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/def_byGensim2.py", encoding='utf-8').read())

# 导入数据
data_tf = pd.read_csv('建模数据.csv', encoding='utf-8')
data_tf = data_tf[['编号', '所属系统', "产品名称", "故障部位", "深层现象", "故障现象", "故障分析", "处理方式标签2", "系统标签"]]
data_tf.columns = ['id', 'system', 'productname', 'faultsite', 'deepph', 'ph', 'analyze', 'handle', 'sysnum']
data_tf.index = pd.Series(range(0, len(data_tf)))
data_tf['phadd'] = data_tf['ph'] + ";" + data_tf['deepph']

# 数据选择
data_tf = data_tf[data_tf.sysnum>2]
data_tf = data_tf[data_tf.handle<4]

# 变量选择
variable = "ph"

# 参数选择
length = 400
LDA_num_topics = 400
word2vec_vector_size = 400
doc2vec_vector_size = 400
word2vec_window = 10
doc2vec_window = 10

# # 描述性统计

# # 先看一下有几个系统，有几个产品，有几个部位

# # 所属系统
# len(data_tf['system'].unique())
# # 共有10类
# count_system = data_tf['system'].value_counts()
# print(count_system)
# # 原始数据：
# # 飞行器平台248；地面站、车辆122；动力装置系统116；导航飞控系统103；侦查系统74；测控系统63；导弹飞控系统62；动力系统37；电气系统30；电器系统22




# # 产品名称
# len(data_tf['productname'].unique())
# # 共有140类/143
# count_productname = data_tf['productname'].value_counts()
# print(count_productname)
# # 原始数据：
# # 飞行器平台150；光电侦察平台54；螺旋桨51；发动机44；主控制盒28；冷却风罩24；速率陀螺组件24；大气数据计算机24；
# # 原始数据_修改：
# # 飞行器平台133；光电侦察平台73；螺旋桨51；发动机44；主控制盒28；冷却风罩24；速率陀螺组件24；大气数据计算机24；

# # 故障部位
# len(data_tf['faultsite'].unique())
# # 共有350类
# count_faultsite = data_tf['faultsite'].value_counts()
# print(count_faultsite)
# # 原始数据：
# # 软件25；冷却风罩24；桨距电机20；托板螺母15；弹垫14；短舱下罩12；防护贴12


# 构建分词数据

# # 选取数据
# data_tf_slice = data_tf.set_index(data_tf['productname'])
# # indexname = ['飞行器平台', '光电侦察平台', '螺旋桨', '发动机', '主控制盒', '冷却风罩', '速率陀螺组件', '大气数据计算机']
# indexname = ['飞行器平台', '光电平台', '螺旋桨', '发动机', '主控制盒', '冷却风罩', '速率陀螺组件', '大气数据计算机']
# data_tf_slice = data_tf_slice.loc[indexname, ]


# 分词

# # 加载停用词表
# stopwords = stopwordslist(r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/stopwords/bd_hit_scu_stopwords.txt")

# 对故障现象(ph)进行分词
data_cut_ph = data_tf[variable]
list_result_cut_ph = simpleWordCut(pd.Series.tolist(data_cut_ph.astype(str)), 
                                   dict = r"C:/Users/ylc/GitHub/Research/research2-21-TextMiningFault/字典_utf8.csv",
                                   stopwords=False,
                                   cut_all=False,
                                   HMM=False)

# 提取TF-IDF特征向量
result_tfidf = tfidf_Gensim(list_result_cut_ph, length=length)
result_tfidf = pd.DataFrame(result_tfidf)


# 提取LDA特征向量
result_LDA = LDA_Gensim(list_result_cut_ph, tfidf=False, num_topics=LDA_num_topics)
result_LDA = pd.DataFrame(result_LDA)


# 提取word2vec特征向量
model_word2vec = WORD2VEC_Gensim(list_result_cut_ph, sg=0, vector_size=word2vec_vector_size,
                                 window=word2vec_window, min_count=0, negative=0, sample=0.001,
                                 hs=0, workers=4, epochs=100)
result_word2vec = featureVec_2vec(list_result_cut_ph, model_word2vec)
result_word2vec = pd.DataFrame(result_word2vec)


# 提取doc2vec特征向量
model_doc2vec = DOC2VEC_Gensim(list_result_cut_ph, dm=0, vector_size=doc2vec_vector_size,
                               window=doc2vec_window, min_count=0,
                               workers=4, epochs=100,
                               hs=0, negative=0)
result_doc2vec = pd.DataFrame(model_doc2vec.dv.vectors)

# # 提取tfidf和word2vec特征向量
# # 对两个特征向量均进行归一化
# result_tfidf_nor = (result_tfidf - result_tfidf.mean()) / result_tfidf.std()
# result_word2vec_nor = (result_word2vec - result_word2vec.mean()) / result_word2vec.std()
# # 串联
# result_tfidfword2vec_series = pd.concat([result_tfidf_nor, result_word2vec_nor], axis=1)
# # 并联加
# result_tfidfword2vec_parallel = result_tfidf_nor + result_word2vec_nor
# # 并联乘
# result_tfidfword2vec_multiply = np.multiply(result_tfidf_nor, result_word2vec_nor)

# # 保存特征结果
# def df_result2data(result_data, y):

#     ncol = result_data.columns.size
#     name1 = 'x'
#     name2 = list(range(1, ncol+1))
#     name = []
#     for i in name2:
#         name.append(name1 + str(i))
#     result_data.columns = name
#     y = list(y)
#     result_data['y'] = y
#     name = ['y'] + name
#     result_data = result_data[name]
    
#     return result_data

# result_data_tfidf = df_result2data(result_tfidf, data_tf['handle'])
# result_data_lda = df_result2data(result_LDA, data_tf['handle'])
# result_data_word2vec = df_result2data(result_word2vec, data_tf['handle'])
# result_data_doc2vec = df_result2data(result_doc2vec, data_tf['handle'])
# result_data_tfidfword2vec_series = df_result2data(result_tfidfword2vec_series, data_tf['handle'])
# result_data_tfidfword2vec_parallel = df_result2data(result_tfidfword2vec_parallel, data_tf['handle'])
# result_data_tfidfword2vec_multiply = df_result2data(result_tfidfword2vec_multiply, data_tf['handle'])

# # result_data_tfidf = df_result2data(result_tfidf, data_tf['sysnum'])
# # result_data_lda = df_result2data(result_LDA, data_tf['sysnum'])
# # result_data_word2vec = df_result2data(result_word2vec, data_tf['sysnum'])
# # result_data_doc2vec = df_result2data(result_doc2vec, data_tf['sysnum'])
# # result_data_tfidfword2vec_series = df_result2data(result_tfidfword2vec_series, data_tf['sysnum'])
# # result_data_tfidfword2vec_parallel = df_result2data(result_tfidfword2vec_parallel, data_tf['sysnum'])
# # result_data_tfidfword2vec_multiply = df_result2data(result_tfidfword2vec_multiply, data_tf['sysnum'])

# 输出特征矩阵
result_tfidf.to_csv("cut_tfidf"+"_"+variable+"_"+str(length)+".csv", index=False)
result_LDA.to_csv("cut_lda"+"_"+variable+"_"+str(LDA_num_topics)+".csv", index=False)
result_word2vec.to_csv("cut_word2vec"+"_"+variable+"_"+str(word2vec_vector_size)+"_"+str(word2vec_window)+".csv", index=False)
result_doc2vec.to_csv("cut_doc2vec"+"_"+variable+"_"+str(doc2vec_vector_size)+"_"+str(doc2vec_window)+".csv", index=False)
# result_tfidfword2vec_series.to_csv("cut_tfidfword2vec"+"_"+variable+".csv", index=False)
# result_data_tfidfword2vec_parallel.to_csv("tfidfwordp.csv", index=False)
# result_data_tfidfword2vec_multiply.to_csv("tfidfwordm.csv", index=False)

# 输出标签
table_handle = pd.DataFrame(data_tf.handle)
table_system = pd.DataFrame(data_tf.sysnum)
table_handle.to_csv("table_handle.csv", index=False)
table_system.to_csv("table_system.csv", index=False)