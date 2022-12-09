library("rjson")
library(data.table)
library(rlist)
library(dplyr)
library(proxy)
library(stringr)
library(textreuse)
library(stringi)
library(tictoc)
library(ggplot2)

# ---- Working directory  
dir <- "/Users/ottohaanappel/Desktop/Master/Computer Science for Business Analytics/"
setwd(dir)

raw_data <- fromJSON(file = "TVs-all-merged.json")

source("Functions.R")

# Convert nested lists (containing the duplicates) into full list of products
list = get_list(raw_data)

final_results = results_r

tic()
for (i in 1:5)
{
  print("Bootstrap:")
  print(i)
  # Bootstrap the data set
  data = bootstrap(list)
  train = data[[1]]
  test = data[[2]]
  
  # Extract model ID's from train and test sets
  modelID_train = get_sublist(train, 3)
  modelID_test = get_sublist(test, 3)
  
  # Find the true duplicates in train and test sets
  duplicates_train = true_duplicates(modelID_train)
  duplicates_test = true_duplicates(modelID_test)
  
  # Extract titles from train and test sets
  titles_train = get_sublist(train, 5)
  titles_test = get_sublist(test, 5)
  
  # Extract specific feature information
  features_train = get_sublist(train, 4)
  features_test = get_sublist(test, 4)
  
  # Clean the titles
  replace_inch_hz = c(' inch' = 'inch', 'inch ' = 'inch','Inch' = 'inch', 
                      'inches' = 'inch', '"' = 'inch','-inch' = 'inch', 
                      'INCH' = 'inch', '-Inch' = 'inch', '”' = 'inch', "'" = 'inch',
                      ' hz' = 'hz', 'hz ' = 'hz', 'Hertz' = 'hz','HZ' = 'hz',
                      'Hz' = 'hz', '-hz' = 'hz', 'hz' = 'hz')
  replace_weight = c(' pounds' = 'lbs', ' lbs' = 'lbs', ' lbs.' = 'lbs')
  extract_brand = c('SuperSonic'='supersonic','Philips'='philips','Sharp'='sharp', 
                    'Samsung'='samsung','NEC'='nec','Toshiba'='toshiba','Hisense'='hisense',       
                    'Sony'='sony','LG'='lg','Sanyo'='sanyo','Coby'='coby', 
                    'Panasonic'='panasonic','Sansui'='sansui','Vizio'='vizio',         
                    'ViewSonic'='viewsonic','SunBriteTV'='sunbritetv','VIZIO' = 'vizio',
                    'Haier'='haier','Optoma'='optoma','Proscan'='proscan','JVC'='jvc',           
                    'TOSHIBA'='toshiba','Supersonic'='supersonic','Pyle'='pyle',
                    'LG Electronics'='lg','Sceptre'='sceptre', 'Magnavox'='magnavox',
                    'Mitsubishi'='mitsubishi','Compaq'='compaq', 'Hannspree'='hannspree',
                    'HANNspree'='hannspree','Sceptre Inc.'='sceptre','UpStar'='upstar',      
                    'Seiki'='seiki','RCA'='rca','Craig'='craig','Affinity'='affinity',          
                    'JVC TV'='jvc','Naxa'='naxa','Westinghouse'='westinghouse',
                    'Epson'='epson','HP'='hp','Elo'='elo','Pansonic'='panasonic',            
                    'Hello Kitty'='hellokitty','SIGMAC'='sigmac','Dynex'='dynex',
                    'Insignia'='insignia','Avue'='avue','Venturer'='venturer','TCL'='tcl',
                    'Viore'='viore','Elite'='elite','CurtisYoung'='curtisyoung', 
                    'Azend'='azend','Hiteker'='hiteker','Contex'='contex','ProScan'='proscan', 'GPX'='gpx')
  
  title_train_clean = clean_text(titles_train, replace_inch_hz)
  title_test_clean = clean_text(titles_test, replace_inch_hz)
  
  brands_train = get_brands(title_train_clean, extract_brand)
  brands_test = get_brands(title_test_clean, extract_brand)
  
  features_train_clean = clean_text(features_train, replace_weight)
  features_test_clean = clean_text(features_test, replace_weight)
  
  # Get model words 
  regex = "[a-zA-Z0-9]*(([0-9]+[^0-9, ]+)|([^0-9, ]+[0-9]+))[a-zA-Z0-9]*"
  lbs_regex = "^[0-9]*(?:lbs)"
  
  model_words_title_train = get_model_words_title(title_train_clean, regex)
  model_words_title_test = get_model_words_title(title_test_clean, regex)
  model_words_features_train = get_model_words_features(features_train_clean, lbs_regex)
  model_words_features_test = get_model_words_features(features_test_clean, lbs_regex)
  
  model_words_train = combine_model_words(model_words_title_train, model_words_features_train, brands_train)
  model_words_test = combine_model_words(model_words_title_test, model_words_features_test, brands_test)
  
  model_words_list_train = list(unique(unlist(model_words_train)))
  model_words_list_test = list(unique(unlist(model_words_test)))
  
  # Make binary matrix
  binary_matrix_train = binary_matrix(model_words_list_train, model_words_train, train)
  binary_matrix_test = binary_matrix(model_words_list_test, model_words_test, test)
  
  ### TRAIN SAMPLE ###
  
  # Minhashing
  nrow_train = length(binary_matrix_train[,1])
  ncol_train = length(binary_matrix_train[1,])
  print(0.5*nrow_train)
  n_hash = 600
  
  possible_comparisons = ncol_train*(ncol_train-1)/2
  
  print("Minhash")
  minhash_train = minhash(n_hash, nrow_train, ncol_train, model_words_list_train, binary_matrix_train)
  
  results_r = list()
  
  r_values = c(5,8,10,15,25,50)
  for (i in 1:length(r_values))
  {
    print(i)
    r = r_values[i]
    # Make the bands
    print("Bands")
    b = n_hash/r
    bands_train = get_bands(b, r, n_hash, minhash_train)
    
    # Make and fill the buckets according to the derived bands
    print("Buckets")
    buckets_train = get_buckets(b, bands_train)
    
    # Get candidate pairs from the buckets
    candidates_train = get_candidates(b, buckets_train)
  
    # Get predicted similar pair according to Jaccard threshold
    #threshold = (1/b)^(1/r)
    resultsf1 = list()
    resultsf1star = list()
    results_pairq = list()
    results_pairc = list()
    results_foc = list()
    
    t = c(0.5, 0.6, 0.7, 0.8, 0.9)
    for (j in 1:length(t))
    {
      thres = t[j]
      similar_pairs_train = get_similar_pairs(candidates_train, binary_matrix_train, thres)
      
      # Calculate evaluation metrics
      f1_train = calculate_f1(similar_pairs_train, duplicates_train)
      f1_star_train = calculate_f1_star(similar_pairs_train, candidates_train, duplicates_train)
      tp = length(intersect(similar_pairs_train, duplicates_train))
      pairq = tp/length(candidates_train)
      pairc = tp/length(duplicates_train)
      foc = length(candidates_train)/possible_comparisons
      
      # Add results for specific threshold to list
      resultsf1[[as.character(thres)]] = append(resultsf1[[as.character(thres)]], f1_train)
      resultsf1star[[as.character(thres)]] = append(resultsf1star[[as.character(thres)]], f1_star_train)
      results_foc[[as.character(thres)]] = append(results_foc[[as.character(thres)]], foc)
      results_pairq[[as.character(thres)]] = append(results_pairq[[as.character(thres)]], pairq)
      results_pairc[[as.character(thres)]] = append(results_pairc[[as.character(thres)]], pairc) 
      results = list(resultsf1, resultsf1star, results_foc, results_pairq, results_pairc)
    }
    results_r[[as.character(r)]] = append(results_r[[as.character(r)]], results)
  }
  for (i in 1:length(results_r))
  {
    for (j in 1:length(results_r[[i]]))
    {
      for (k in 1:length(results_r[[i]][[j]]))
      {
        final_results[[i]][[j]][[k]] = append(final_results[[i]][[j]][[k]], results_r[[i]][[j]][[k]])
      }
    }
  }
}
toc()


### TEST SAMPLE ###

# Minhashing
nrow_test = length(binary_matrix_test[,1])
ncol_test = length(binary_matrix_test[1,])
#n_hash = floor(0.5*nrow_test)
n_hash = 480

minhash_test = minhash(n_hash, nrow_test, ncol_test, model_words_list_test, binary_matrix_test)

r = r_test
b = n_hash/r

bands_test = get_bands(b, r, n_hash, minhash_test)

# Make and fill the buckets according to the derived bands
buckets_test = get_buckets(b, bands_test)

# Get candidate pairs from the buckets
candidates_test = get_candidates(b, buckets_test)

# Get predicted similar pair according to Jaccard threshold
similar_pairs_test = get_similar_pairs(candidates_test, binary_matrix_test, 0.6)

# Calculate evaluation metrics
f1_test = calculate_f1(similar_pairs_test, duplicates_test)
f1_star_test = calculate_f1_star(similar_pairs_test ,candidates_test, duplicates_test)
results = rbind(results, c(f1_test, f1_star_test))

# Plots
Fraction_of_Comparisons = c(3.864158e-05, 6.848359e-05, 0.0002486834, 0.001185646, 0.003195773, 0.03557053)
Pair_Quality = c(0.468, 0.510, 0.325, 0.123, 0.046, 0.004)
Pair_Completeness = c(0.060, 0.114, 0.268, 0.457, 0.475, 0.476)
F1_measure = c(0.106, 0.186, 0.320, 0.407, 0.413, 0.414)
F1star_measure = c(0.106, 0.186, 0.293, 0.193, 0.084, 0.008)

plot(Fraction_of_Comparisons, Pair_Quality, lines(Fraction_of_Comparisons, Pair_Quality))
plot(Fraction_of_Comparisons, Pair_Completeness, lines(Fraction_of_Comparisons, Pair_Completeness))
plot(Fraction_of_Comparisons, F1_measure, lines(Fraction_of_Comparisons, F1_measure))
plot(Fraction_of_Comparisons, F1star_measure, lines(Fraction_of_Comparisons, F1star_measure))

