
# coding: utf-8

# In[1]:


import gensim
import pandas as pd
import numpy as np
import re
from scipy.stats import spearmanr
from IPython.display import display, HTML


# In[2]:


# S'il vous plait ne pas executez ce bloc, il est deja chargé

# Loading GoogleNews pre-trained model
model_GoogleNews = gensim.models.KeyedVectors.load_word2vec_format('GoogleNews-vectors-negative300.bin', binary=True)

# Loading Text8 pre-trained-model
# sentences = gensim.models.word2vec.Text8Corpus('text8')
# model_text8 = gensim.models.Word2Vec(sentences, size=300)
# model_text8.save('text8.300d')
model_text8 = gensim.models.Word2Vec.load('text8.300d')

# Loading Glove
# glove_word2vec = gensim.scripts.glove2word2vec.glove2word2vec('glove.840B.300d.txt', 'glove.840B.300d.txt.word2vec')
model_glove = gensim.models.KeyedVectors.load_word2vec_format('glove.840B.300d.txt.word2vec')


# In[30]:


def split_into_words(d, lower=False):
    d = re.sub(r"\W+", " ", d)
    if lower:
        d = d.lower()
    return d.split()


# In[33]:


def read_dataset(path_dataset, pos_word1=0, pos_word2=1, pos_correlation=2, header=None, sep=';', engine='python'):
    """
    """
    tmp = pd.read_csv(path_dataset, header=header, sep=sep, engine=engine)
    tmp = tmp.as_matrix()
    data = np.zeros_like(tmp)
    data[:, 0] = tmp[:, pos_word1]
    data[:, 1] = tmp[:, pos_word2]
    data[:, 2] = tmp[:, pos_correlation]
    data = data[:, 0:3]
    return data


# In[26]:


def similarity(model, splitted_word1, splitted_word2):
    """
    """
    score = 0.0
    try:
        score = model.n_similarity(splitted_word1, splitted_word2)
    except:
    # if words are not in vocabulary -> similarity = 0
        return 0.0
                
    return score


# In[27]:


def spearman_coefficient(dataset, model, lower=False):
    """
    Input:
    - model: word embedding model
    - dataset: matrix type, dimension (N, 3)
        - First and second column: words pair
        - Third column: similarity assigned by human
    
    Output:
    - Spearman's rank correlation coefficient
    """
    N = dataset.shape[0]
    similarities_by_human = dataset[:, 2]
    similarities_by_model = np.zeros(N)
    
    for i in range(N):
        word0 = dataset[i, 0]
        word1 = dataset[i, 1]
        # keep original words
        splitted_word0 = split_into_words(word0)
        splitted_word1 = split_into_words(word1)
        similarity1 = similarity(model, splitted_word0, splitted_word1)
        similarities_by_model[i] = similarity1

        if lower:
            # lowercase all terms
            splitted_word0 = split_into_words(word0, lower=True)
            splitted_word1 = split_into_words(word1, lower=True)
            similarity2 = similarity(model, splitted_word0, splitted_word1)
            similarities_by_model[i] = max(similarity1, similarity2)
        
    
    return spearmanr(similarities_by_human, similarities_by_model)[0]


# In[6]:


# try something fun
model_text8.similarity('fax', 'email')


# In[7]:


model_GoogleNews.n_similarity(['urban', 'area'], ['city'])


# In[8]:


model_glove.similarity('area', 'city')


# In[9]:


model_glove.most_similar(positive=['woman', 'king'], negative=['man'])


# In[11]:


# Datasets
wordsim = read_dataset('wordsim.csv')
mc = read_dataset('mc.csv')
MTURK_771 = read_dataset('MTURK-771.csv', sep=',')
rg = read_dataset('rg.csv')
rel122_norms = read_dataset('rel122-norms.txt', sep='  | -- ', pos_word1 = 1, pos_word2 = 2, pos_correlation = 0)
simlex_999 = read_dataset('SimLex-999.txt', header=0, sep='\t', pos_word1 = 0, pos_word2 = 1, pos_correlation = 7)
UMNSRS_similarity = read_dataset('UMNSRS_similarity.csv', header=0, sep=',', pos_word1 = 2, pos_word2 = 3, pos_correlation= 0)
UMNSRS_relatedness = read_dataset('UMNSRS_relatedness.csv', header=0, sep=',', pos_word1 = 2, pos_word2 = 3, pos_correlation = 0)


# In[12]:


# cos_matrix_brm_IFR
cos_matrix_brm_IFR = pd.ExcelFile('cos_matrix_brm_IFR.xlsx')

def remove_special_chars(a):
    a = a.replace('_(',' ')
    a = a.replace(')','')
    return a

cos_matrix_tmp = pd.DataFrame()

for sheet in cos_matrix_brm_IFR.sheet_names:
    df = cos_matrix_brm_IFR.parse(sheet)
    cos_matrix_tmp = pd.concat([cos_matrix_tmp, df], axis=1)

cos_matrix_tmp.drop('CONCEPT', axis=1, inplace=True)
cos_matrix = []
columns_name = cos_matrix_tmp.columns.values
for i, row in cos_matrix_tmp.iterrows():
    for j in range(i + 1, columns_name.shape[0]):
        if row[columns_name[j]] > 0.0:
            cos_matrix.append([remove_special_chars(columns_name[i]), 
                               remove_special_chars(columns_name[j]), 
                               row[columns_name[j]]])
cos_matrix = np.array(cos_matrix)


# In[13]:


datasets = [wordsim, 
            mc, 
            MTURK_771, 
            rg, rel122_norms, 
            simlex_999, 
            UMNSRS_relatedness, 
            UMNSRS_similarity, 
            cos_matrix]

dataset_names = ['wordsim', 
                 'mc', 
                 'MTURK-771', 
                 'rg', 
                 'rel122-norms', 
                 'SimLex-999', 
                 'UMNSRS_relatedness', 
                 'UMNSRS_similarity', 
                 'cos_matrix_brm_IFR']

models = [model_text8, model_GoogleNews, model_glove]
model_names = ['text8', 'GoogleNews', 'glove']

results = {}
results['Model'] = ['text8', 'GoogleNews', 'glove']
for i in range(len(datasets)):
    coefficients = []
    for model in models:
        coefficients.append(spearman_coefficient(datasets[i], model, lower=True))
    
    results[dataset_names[i]] = coefficients


# In[14]:


x = pd.DataFrame(results)

# Re-ordering the columns
x = x[['Model', 'MTURK-771', 'cos_matrix_brm_IFR', 'rel122-norms', 'mc', 'rg', 'wordsim', 'SimLex-999', 'UMNSRS_relatedness', 'UMNSRS_similarity']]
x


# In[19]:


print(model_glove.most_similar(positive=['Baghdad', 'Greece'], negative=['Athen'])[0])


# In[21]:


print(model_GoogleNews.most_similar(positive=['Baghdad', 'Greece'], negative=['Athen']))


# In[16]:


print(model_text8.most_similar(positive=['king', 'woman'], negative=['queen']))


# In[33]:


# Google Question
# Read dataset
google_questions = pd.read_csv('questions-words.txt', header=None, sep=' ', comment=':')
google_questions.head()
google_questions = google_questions.as_matrix()


# In[34]:



google_questions


# In[ ]:


human_answer = google_questions[:, 3]
accuracy = {}
for i in range(len(models)):
    answer = []
    for j in range(google_questions.shape[0]):
        try:
            answer.append(models[i].most_similar(positive=[google_questions[j, 1], 
                                                       google_questions[j, 2]], 
                                             negative=[google_questions[j, 0]])[0][0])
        except:
            answer.append('_')
    answer = np.array(answer)
    accuracy[model_names[i]] = (answer == human_answer).mean()


# In[38]:


model_names = ['text8', 'GoogleNews', 'glove']


# In[40]:


a = np.array(['aa', 'bb', 'c'])
b = np.array(['aa', 'b', 'c'])
(a == b).mean()


# In[4]:


a = model_text8.accuracy('questions-words.txt')


# In[29]:


a[1]['section']


# In[31]:


def split_into_words(d, lower=False):
    d = re.sub(r"\W+", " ", d)
    if lower:
        d = d.lower()
    return d.split()


# In[32]:


split_into_words('bat_(animal)')

