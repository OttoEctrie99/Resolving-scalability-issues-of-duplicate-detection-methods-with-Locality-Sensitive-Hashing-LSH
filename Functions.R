# Function that extracts single lists from nested list
get_list = function (raw_data)
{
  list = list()
  for (i in 1:length(raw_data))
  {
    for (j in 1:length(raw_data[[i]]))
    {
      list = list.append(list, raw_data[[i]][[j]])
    }
  }
  return(list)
}

get_sublist = function (list, e)
{
  sublist = list()
  for (i in 1:length(list))
  {
    sublist = list.append(sublist, list[[i]][[e]])
  }
  return (sublist)
}

# Function to bootstrap the data into train (63%) and test (37%) set
bootstrap = function (list)
{
  train = list()
  test = list()
  dt = sort(sample(length(list), length(list)*.63))
  dt2 = setdiff(1:1624, dt)
  for (i in dt)
  {
    train = list.append(train, list[[i]])
  }
  for (i in dt2)
  {
    test = list.append(test, list[[i]])
  }
  return (list(train, test))
} 

# Function that finds true duplicates
true_duplicates = function (modelIDlist)
{
  duplicates = list()
  for (i in 1:length(modelIDlist))
  {
    for (j in 1:length(modelIDlist))
    {
      if (i != j)
      {
        if (modelIDlist[[i]] == modelIDlist[[j]])
        {
          pair = sort(c(i, j))
          duplicates = list.append(duplicates, pair)
        }
      }
    }
  }
  duplicates = unique(duplicates)
  return (duplicates)
}

# Function to clean the text in titles
clean_text = function (title_list, replace_inch_hz)
{
  title_list_clean = lapply(title_list, function(x) {
    text = str_replace_all(x, replace_inch_hz)
    text = tolower(gsub("[[:punct:]]", "", text))
    text = str_trim(gsub("\\s+", " ", text)) 
    #text = unlist(strsplit(text, " "))
    return(text)
  })
  return (title_list_clean)
}

# Function to extract clean model words from titles
get_model_words_title = function (title_list_clean, regex)
{
  model_words = list()
  for (i in 1:length(title_list_clean))
  {
    title = str_extract_all(title_list_clean[[i]], regex)
    title = stri_remove_empty(str_remove_all(title[[1]], "[0-9]{4}(?:inch)|[0-9]{3}(?:inch)"))
    model_words = list.append(model_words, title)
  }
  model_words = lapply(model_words, function(x) {unique(x)})
  return (model_words)
}

# Function to extract clean model words from features map
get_model_words_features = function (features_list_clean, regex)
{
  model_words = list()
  for (i in 1:length(features_list_clean))
  {
    feature = unlist(str_extract_all(features_list_clean[[i]], regex))
    model_words = list.append(model_words, feature)
  }
  model_words = lapply(model_words, function(x) {unique(x)})
  return (model_words)
}

get_brands = function (title_list_clean, replace_brands)
{
  brands = list()
  for (i in 1:length(title_list_clean))
  {
    brand = unique(stri_remove_empty_na(str_extract(title_list_clean[[i]], extract_brand)))
    if (length(brand)>0)
    {
      brands = list.append(brands, brand[1])
    }
    else 
    {
      brands = list.append(brands, "unknown")
    }
  }
  return (brands)
}

combine_model_words = function (model_words_title_train, model_words_features_train, brands_train)
{
  model_words = list()
  for (i in 1:length(model_words_title_train))
  {
    model_words_i = list()
    model_words_i = append(model_words_i, model_words_title_train[[i]])
    model_words_i = append(model_words_i, model_words_features_train[[i]])
    if (length(brands_train[[i]])>0){
    model_words_i = append(model_words_i, brands_train[[i]])}
    model_words = list.append(model_words, model_words_i)
  }
  return (model_words)
}

# Function that creates the binary characteristics matrix
binary_matrix = function (model_words_list, model_words, list)
{
  word_matrix = matrix(0, length(model_words_list[[1]]), length(list))
  for (i in 1:length(list))
  {
    for (j in 1:length(model_words_list[[1]]))
    {
      for (k in 1:length(model_words[[i]]))
      {
        if (model_words_list[[1]][j] == model_words[[i]][k])
        {
          word_matrix[j,i] = 1
        }
        else if (word_matrix[j,i] != 1)
        {
          word_matrix[j,i] = 0
        }
      }
    }
  }
  word_matrix = as.data.frame(word_matrix)
  return (word_matrix)
}

# Hash function
h = function(a,x, nrow)
{
  h_i = (a[1]*x+a[2]) %% nrow
  return (h_i)
}

# Function that minhashes the binary matrix
minhash = function (n_hash, nrow, ncol, model_words_list, binary_matrix)
{
  hash_comb = matrix(0, nrow = n_hash, ncol = 2)
  for (i in 1:n_hash)
  {
    a = sample(1:length(model_words_list[[1]]), 2)
    hash_comb[i,] = a
  }
  M = matrix(1000000000, nrow = n_hash, ncol = ncol)
  H = matrix(0, nrow = n_hash, ncol = 1)
  for (i in 1:nrow)
  {
    for (j in 1:n_hash)
    {
      H[j,1] = h(hash_comb[j,],i, nrow)
    }
    for (c in 1:ncol)
    {
      if (binary_matrix[i,c] == 1)
      {
        for (k in 1:n_hash)
        {
          if (H[k,1] < M[k,c])
          {
            M[k,c] = H[k,1]
          }
        }
      }
    }
  }
  return (M)
}

# Function that makes the bands
get_bands = function (b,r, n_hash, M)
{
  bands = list()
  for (b in 1:b)
  {
    bands[[b]] = list()
  }
  for (s in 1:length(M[1,]))
  {
    for (i in 1:b)
    {
      idx = i*r
      for (k in 1:r)
      {
        if (idx <= n_hash)
        {
          band_string = character()
          string = gsub("\\s+", " ", M[(idx-r+1):idx,s])
          for (str in string)
          {
            band_string = paste(band_string,str, sep = "")
          }
          bands[[i]][[s]] = band_string
        }
      }
    }
  }
  return (bands)
}

# Function that creates and fills the buckets
get_buckets = function (b, bands)
{
  buckets = list()
  for (b in 1:b)
  {
    buckets[[b]] = list()
  }
  for (b in 1:b)
  {
    buckets[[b]][[bands[[1]][[1]]]][[1]] = as.integer(1)
    for (d in 2:length(bands[[1]]))
    {
      bandid = bands[[b]][[d]]
      for (buck in list.names(buckets[[b]]))
      {
        if (bandid != buck && length(buckets[[b]][[bandid]]) == 0)
        {
          buckets[[b]][[bandid]][[1]] = d
        }
        else if (bandid == buck)
        {
          buckets[[b]][[bandid]] = append(buckets[[b]][[bandid]], d)
        }
      }
    }
  }
  return (buckets)
}

# Function that gets the candidates from the buckets
get_candidates = function (b, buckets)
{
  candidates = list()
  for (b in 1:b)
  {
    for (i in 1:length(buckets[[b]]))
    {
      if (length(buckets[[b]][[i]]) > 1)
      {
        for (l in 1:(length(buckets[[b]][[i]])-1))
        {
          for (k in (l+1):length(buckets[[b]][[i]]))
          {
            pair = c(buckets[[b]][[i]][[l]],buckets[[b]][[i]][[k]])
            candidates = list.append(candidates, pair)
          }
        }
      }
    }
  }
  candidates = unique(candidates)
  return (candidates)
}

# Jaccard similarity function on sets
jaccard <- function(x, y) {
  set_intersection <- length(intersect(x, y))
  set_union <- length(union(x, y))
  return(set_intersection / set_union)
}

# Jaccard similarity on (binary) columns
column_jaccard <-  function(c1, c2) {
  non_zero <- which(c1 | c2)
  column_intersect <- sum(c1[non_zero] & c2[non_zero])
  column_union <- length(non_zero)
  return(column_intersect / column_union)
}

# Function that extracts the predicted similar pairs according to Jaccard threshold
get_similar_pairs = function (candidates, binary_matrix, threshold)
{
  similar_pairs = list()
  for (pair in candidates)
  {
    product1 = pair[1]
    product2 = pair[2]
    jaccard_value = column_jaccard(binary_matrix[,product1], binary_matrix[,product2])
    if (jaccard_value >= threshold)
    {
      similar_pairs = list.append(similar_pairs, pair)
    }
  }
  return (similar_pairs)
}

# Function for F1 metric
calculate_f1 = function (similar_pairs, duplicates)
{
  tp = length(intersect(similar_pairs, duplicates))
  fp = length(setdiff(similar_pairs, duplicates))
  fn = length(setdiff(duplicates, intersect(similar_pairs, duplicates)))
  recall = tp/(tp + fn)
  precision = tp/(tp + fp)
  f1_score = 2*(recall*precision)/(recall + precision)
  return (f1_score)
}
# Function for F1* metric
calculate_f1_star = function (similar_pairs, candidates, duplicates)
{
  tp = length(intersect(similar_pairs, duplicates))
  pairq = tp/length(candidates)
  pairc = tp/length(duplicates)
  f1_star = 2*(pairq*pairc)/(pairq + pairc)
  return (f1_star)
}
