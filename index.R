library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(seededlda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(forcats)

dcinbox_nl <- read_csv("data/dcinbox_full.csv")
# dcinbox_nl <- read_csv("data/dcinbox.csv")
# dcinbox_nl

# check if column body has any missing values
sum(is.na(dcinbox_nl$body))



# Measure distance or similarity from a dfm
# textstat_simil(x, "2017-Trump", method = "cosine", 
#   margin = c("documents", "features"))


# convert text to lowercase
new_col_names <- tolower(names(dcinbox_nl))
dcinbox_nl <- setNames(dcinbox_nl, new_col_names)
names(dcinbox_nl)[names(dcinbox_nl) == 'unix timestamp'] <- 'date'
dcinbox_nl$date <- dcinbox_nl$date / 1000

dcinbox_nl <- dcinbox_nl[dcinbox_nl$party %in% c("Democrat", "Republican"), ]

# convert unix timestamp to date
dcinbox_nl$date <- as.POSIXct(dcinbox_nl$date, origin="1970-01-01")

# Extract the date component
dcinbox_nl$year <- format(dcinbox_nl$date, "%Y")
dcinbox_nl$date <- format(dcinbox_nl$date, "%Y-%m-%d")


# add a presidency column with the president's name
dcinbox_nl$presidency <- ifelse(dcinbox_nl$date < "2017-01-19", "Obama", ifelse(dcinbox_nl$date < "2021-01-19", "Trump", "Biden"))

# DETERMINE CLASS HEALTHCARE
set_insurance = c('Medicare', 'Medicaid', 'Medical', 'insurance', 'Coverage', 'Care', 'ACA')
set_healthcare = c('Doctor', 'Hospital', 'Nurse', 'Patient', 'Medicine', 'Treatment', 'Diagnosis', 'Surgery', 'Healthcare', 'Clinic', 'Emergency', 'Pharmacy', 'Ambulance', 'Rehabilitation', 'Laboratory', 'Vaccination', 'Therapy', 'Disease', 'Prevention')
set_health = c('Nutrition', 'Exercise', 'Healthy', 'Health', 'Wellness', 'Lifestyle', 'Well-being', 'Vitality', 'Chronic', 'mental', 'Holistic', 'Physical', 'Immunity', 'Hygiene', 'Meditation')
heathcare_lst = c(set_insurance, set_healthcare, set_health)

# Create a regular expression pattern using the word list
pattern <- paste(heathcare_lst, collapse = "|")

# Use grepl() function to check if the pattern matches any word in the text
dcinbox_nl$healthcare <- ifelse(grepl(pattern, dcinbox_nl$body), 1, 0)

# create a plot that shows number of healthcare related newsletters by party
dcinbox_nl %>%
  group_by(party) %>%
  summarise(healthcare = sum(healthcare)) %>%
  ggplot(aes(x = party, y = healthcare, fill=as.factor(party))) +
  geom_bar(stat = "identity") +
  labs(x = "Party", y = "Number of healthcare related newsletters", title = "Number of healthcare related newsletters by party") +
  theme_minimal() +
  # theme(legend.position = "none") +
  scale_fill_manual(values = c("#0B6993", "#EB4941"))

  # TEST PLOT
  prop <- function(count, group) {
  count / tapply(count, group, sum)[group]
}

# ggplot(dcinbox_nl, aes(
#   x = presidency, y = prop(after_stat(count), after_stat(x)),
#   fill = party, label = scales::percent(prop(after_stat(count), after_stat(x)))
# )) +
#   geom_bar() +
#   geom_text(stat = "count", position = position_fill(vjust = 0.5), colour = "white", size = 5) +
#   labs(y = "Percent", title = "Top Big Cat Populations", x = "Country") +
#   scale_fill_discrete(name = NULL, labels = c("Siberian/Bengal", "Other wild cats", "Puma/Leopard/Jaguar")) +
#   scale_y_continuous(labels = scales::percent)


  # by presidency
ggplot(dcinbox_nl,aes(x = presidency, y = prop(after_stat(count), after_stat(x)),fill = party, label = scales::percent(prop(after_stat(count), after_stat(x))))) + 
# ggplot(dcinbox_nl, aes(x=party, y = prop.table(after_stat(count)), fill=cat_type, label = scales::percent(prop.table(stat(count)))))+
    geom_bar(position = position_fill()) +
    geom_text(stat = "count", position = position_fill(vjust=0.5),colour = "white", size = 5)+
    scale_fill_manual(values = c("#0B6993", "#EB4941")) +
    scale_y_continuous(labels = scales::percent)
    
# Create a corpus of the speeches
corpus_dcinbox <- corpus(dcinbox_nl, text_field = "body")

# # Some common pre-processing
toks <- tokens(corpus_dcinbox, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks <- tokens_select(toks, stopwords("en"), selection = "remove")
toks <- tokens_wordstem(toks)

# ngrams <- tokens_ngrams(toks, n = 2)

dfm <- dfm(toks)
# dfm <- dfm(ngrams)

# We can trim the dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")
dfm_trimmed

# Subset dfm and meatadata to only newsletters labeled as healthcare
dfm_trimmed2 <- dfm_trimmed[dcinbox_nl$healthcare == 1, ]
# metadata <- metadata[metadata$party %in% c("Democrat", "Republican"), ]

# USING QUANTEDA TEXTSTAT_SIMIL
dfm_dem <- dfm_trimmed2[dfm_trimmed2$party == 'Democrat', ]
dfm_rep <- dfm_trimmed2[dfm_trimmed2$party == 'Republican', ]
sum_words_dem <- colSums(dfm_dem)
sum_words_rep <- colSums(dfm_rep)
dfm_dem_single_doc <- as.dfm(t(sum_words_dem))
dfm_rep_single_doc <- as.dfm(t(sum_words_rep))
new_df_trimmed <- rbind(dfm_dem_single_doc, dfm_rep_single_doc)
(tstat1 <- textstat_simil(new_df_trimmed, method = "correlation", margin = "documents"))

dfm_grp <- dfm_group(dfm_trimmed2, groups = presidency)

aux_dfm <- dfm_group(dfm_trimmed, groups = interaction(party, presidency))
# dfm_trump <- n_dfm[n_dfm$presidency == 'Trump', ]
dfm_biden <- n_dfm[n_dfm$presidency == 'Biden', ]
# dfm_obama <- n_dfm[n_dfm$presidency == 'Obama', ]
sum_words_trump <- colSums(dfm_trump)
sum_words_biden <- colSums(dfm_biden)
sum_words_obama <- colSums(dfm_obama)
dfm_trump_single_doc <- as.dfm(t(sum_words_trump))
# docnames(dfm_trump_single_doc) <- "Trump_docs"
dfm_biden_single_doc <- as.dfm(t(sum_words_biden))
# docnames(dfm_biden_single_doc) <- "Biden_docs"
dfm_obama_single_doc <- as.dfm(t(sum_words_obama))
# docnames(dfm_obama_single_doc) <- "Obama_docs"
new_df_trimmed <- rbind(dfm_trump_single_doc, dfm_biden_single_doc, dfm_obama_single_doc)
(tstat1 <- textstat_simil(n_dfm, method = "cosine", margin = "documents"))


# dfm_trump <- dfm_trimmed[dfm_trimmed$presidency == 'Trump', ]
# dfm_biden <- dfm_trimmed[dfm_trimmed$presidency == 'Biden', ]
# dfm_obama <- dfm_trimmed[dfm_trimmed$presidency == 'Obama', ]
# sum_words_trump <- colSums(dfm_trump)
# sum_words_biden <- colSums(dfm_biden)
# sum_words_obama <- colSums(dfm_obama)
# dfm_trump_single_doc <- as.dfm(t(sum_words_trump))
# # docnames(dfm_trump_single_doc) <- "Trump_docs"
# dfm_biden_single_doc <- as.dfm(t(sum_words_biden))
# # docnames(dfm_biden_single_doc) <- "Biden_docs"
# dfm_obama_single_doc <- as.dfm(t(sum_words_obama))
# # docnames(dfm_obama_single_doc) <- "Obama_docs"
# new_df_trimmed <- rbind(dfm_trump_single_doc, dfm_biden_single_doc, dfm_obama_single_doc)
# (tstat1 <- textstat_simil(new_df_trimmed, method = "cosine", margin = "documents"))


# dfm_papers <- dfm_trimmed[dcinbox_nl$party %in% c("Democrat", "Republican"), ]
# fed <- fed[dcinbox_nl$party %in% c("Democrat", "Republican"), ]

# Which words distinguish Republican and Democrat newsletters?
# Mutual Information function
mi <- function(dfm, clust.vect){
  np <- sum(clust.vect)
  ns <- sum(!clust.vect)
  D = np + ns
  nj <- apply(dfm,2,function (x) sum(x>0))
  nnotj <- apply(dfm,2,function (x) sum(x==0))
  njp <- apply(dfm[clust.vect,], 2, function (x) sum(x>0))
  njs <- apply(dfm[!clust.vect,], 2, function (x) sum(x>0))
  nnotjp <- apply(dfm[clust.vect,], 2, function (x) sum(x==0))
  nnotjs <- apply(dfm[!clust.vect,], 2, function (x) sum(x==0))
  mi <- njp/D*log((njp*D)/(np*nj),2)+ njs/D*log((njs*D)/(nj*ns),2) +
    nnotjp/D*log((nnotjp*D)/(np*nnotj),2) +
    nnotjs/D*log((nnotjs*D)/(nnotj*ns),2) 
  names(mi) <- colnames(dfm)
  return(mi)
}

# mi_dem <- mi(dfm_new, metadata_2$party=="Democrat")
mi_dem <- mi(dfm_trimmed2, dfm_trimmed2$party == "Democrat")

# Calculate difference in proportion for x-axis
# np: Total number of hamilton documents
# np <- sum(metadata_2$party=="Democrat")
np <- sum(dcinbox_nl$party == "Democrat")
# ns: Total number of madison documents
# ns <- sum(metadata_2$party=="Republican")
ns <- sum(dcinbox_nl$party == "Republican")
# Number of hamilton documents that contain word j
# njp <- apply(dfm_new[metadata_2$party=="Democrat",], 2, function (x) sum(x>0))
njp <- apply(dfm_trimmed[dcinbox_nl$party == "Democrat",], 2, function (x) sum(x>0))
# Number of madison documents that contain word j
# njs <- apply(dfm_new[metadata_2$party=="Republican",], 2, function (x) sum(x>0))
njs <- apply(dfm_trimmed[dcinbox_nl$party == "Republican",], 2, function (x) sum(x>0))


plot(njp/np-njs/ns, mi_dem, col="black", ylab="Mutual Information",
     xlab="Rep <--> Dem", main="", cex.axis=1.2, cex.lab=1.5)
text(njp/np-njs/ns, mi_dem, names(mi_dem), cex=mi_dem/max(mi_dem, na.rm=T)+.3)

dem_words <- sort(mi_dem[njp/np-njs/ns>0], decreasing=T)[1:20]
rep_words <- sort(mi_dem[njp/np-njs/ns<0], decreasing=T)[1:20]
dem_words
rep_words

# A different perspective: Fightin' words
clusterFightinWords <- function(dfm, clust.vect, alpha.0=100) {
  # we need to get the overall corpus word distribution and the cluster-specific words dists
  # y_{kw} in Monroe et al. 
  overall.terms <- colSums(dfm)
  # n and n_k in Monroe et al. 
  n <- sum(overall.terms)
  # alpha_{kw} in Monroe et al. 
  prior.terms <- overall.terms / n * alpha.0
  # y_{kw}(i) in Monroe et al.
  cluster.terms <- colSums(dfm[clust.vect, ])
  # n_k(i) in Monroe et al.
  cluster.n <- sum(cluster.terms)
  
  cluster.term.odds <- 
    (cluster.terms + prior.terms) / 
    (cluster.n + alpha.0 - cluster.terms - prior.terms)
  overall.term.odds <- 
    (overall.terms + prior.terms) / 
    (n + alpha.0 - overall.terms - prior.terms)
  
  log.odds <- log(cluster.term.odds) - log(overall.term.odds)
  
  variance <- 1/(cluster.terms + prior.terms) + 1/(overall.terms + prior.terms)
  
  # return the variance weighted log-odds for each term
  output <- log.odds / sqrt(variance)
  names(output) <- colnames(dfm)
  return(output)
}

# Find words that are distinctive between speeches written by Republicans versus
# Democrat
# rep_terms <- clusterFightinWords(dfm_new, metadata_2$party == "Republican")
rep_terms <- clusterFightinWords(dfm_trimmed, dcinbox_nl$party == "Republican")
sort(rep_terms, decreasing=T)[1:20]

# dem_terms <- clusterFightinWords(dfm_new, metadata_2$party == "Democrat")
dem_terms <- clusterFightinWords(dfm_trimmed, dcinbox_nl$party == "Democrat")
sort(dem_terms, decreasing=T)[1:20]


# ================================================

# Cosine Similarity function
cosine_sim <- function(a, b) crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))

dem <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Democrat",], 2, sum)
rep <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")%in%c("Republican"),], 2, sum)

cosine_sim(dem, rep)

trump <- apply(dfm_trimmed[docvars(dfm_trimmed, "presidency")=="Trump",], 2, sum)
biden <- apply(dfm_trimmed[docvars(dfm_trimmed, "presidency")=="Biden",], 2, sum)
obama <- apply(dfm_trimmed[docvars(dfm_trimmed, "presidency")=="Obama",], 2, sum)

cosine_sim(trump, biden)
cosine_sim(trump, obama)
cosine_sim(obama, biden)

dfm_trimmed_tfidf <- dfm_tfidf(dfm_trimmed)
dfm_trimmed_tfidf[1,]
dfm_trimmed[1,]

# Create two vectors -- one for Dem and Rep
new_dem <- apply(dfm_trimmed_tfidf[docvars(dfm_trimmed_tfidf, "party") %in% c("Democrat"),], 2, sum)
new_rep <- apply(dfm_trimmed_tfidf[docvars(dfm_trimmed_tfidf, "party") %in% c("Republican"),], 2, sum)

cosine_sim(new_dem, new_rep)

# ===============================
# compare newsletters between different terms of same party
#Clinton and Obama
# date_starts <- as.numeric(as.POSIXct("2022-12-20")) * 1000
# date_ends <- as.numeric(as.POSIXct("2023-01-20")) * 1000
# dfm_dem_1 <- dfm_trimmed[metadata$date < date_ends, ]

# date_starts <- as.numeric(as.POSIXct("2022-12-20")) * 1000
# date_ends <- as.numeric(as.POSIXct("2023-01-20")) * 1000
# dfm_dem_1 <- dfm_trimmed[metadata$date < date_ends, ]

# test
options(scipen=999)
#1s term
date_starts_1 <- as.numeric(as.POSIXct("1993-01-20", origin="1970-01-01"))
date_ends_1 <- as.numeric(as.POSIXct("2001-01-20", origin="1970-01-01"))

# 2nd term
date_starts_2 <- as.numeric(as.POSIXct("2009-01-20", origin="1970-01-01"))
date_ends_2 <- as.numeric(as.POSIXct("2017-01-20", origin="1970-01-01"))

# Specify the desired total length including leading zeros
total_length <- 10

# Add leading zeros to the right using str_pad()
# 1st term
date_starts_1 <- str_pad(as.character(date_starts_1), width = total_length, side = "right", pad = "0")
date_starts_1<- as.numeric(date_starts_1) 
date_ends_1 <- str_pad(as.character(date_ends_1), width = total_length, side = "right", pad = "0")
date_ends_1 <- as.numeric(date_ends_1)

# 2nd term
date_starts_2 <- str_pad(as.character(date_starts_2), width = total_length, side = "right", pad = "0")
date_starts_2<- as.numeric(date_starts_2)
date_ends_2 <- str_pad(as.character(date_ends_2), width = total_length, side = "right", pad = "0")
date_ends_2 <- as.numeric(date_ends_2)

# dem_1 <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Democrat" & (docvars(dfm_trimmed, "date") >= as.POSIXct("2017-05-20") & docvars(dfm_trimmed, "date") <= as.POSIXct("2021-05-20")),], 2, sum)

dem_1 <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Democrat" & (docvars(dfm_trimmed, "date") >= date_starts_1 & docvars(dfm_trimmed, "date") < date_ends_1),], 2, sum)
dem_2 <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Democrat" & (docvars(dfm_trimmed, "date") >= date_starts_2 & docvars(dfm_trimmed, "date") <= date_ends_2),], 2, sum)

# similiarity between Clinton and Obama
cosine_sim(dem_1, dem_2)

rep_1 <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Republican" & (docvars(dfm_trimmed, "date") >= as.numeric(as.POSIXct("2001-01-20")) * 1000 & docvars(dfm_trimmed, "date") < as.numeric(as.POSIXct("2009-01-20")) * 1000),], 2, sum)
rep_2 <- apply(dfm_trimmed[docvars(dfm_trimmed, "party")=="Republican" & (docvars(dfm_trimmed, "date") >= as.numeric(as.POSIXct("2017-12-20")) * 1000 & docvars(dfm_trimmed, "date") <= as.numeric(as.POSIXct("2021-01-20")) * 1000),], 2, sum)

cosine_sim(rep_1, rep_2)

# ===========================================

# # # # # # 
# LDA
# # # # # # 
# Run LDA using quanteda
lda <- textmodel_lda(dfm_trimmed, k = 10)

# Most likely term for each topic
lda.terms <- terms(lda, 20)
lda.terms

# Topical content matrix
mu <- lda$phi
dim(mu) #  10 topics, X words
mu[1:10,1:20]
# Most representative words in Topic 1
mu[1,][order(mu[1,], decreasing=T)][1:10]

# Topical prevalence matrix
pi <- lda$theta
dim(pi) # number of docs by number of topics

# Most representative documents in Topic 1
dcinbox_nl[order(pi[, 1],decreasing=T),] #  there was an error in the previous file

# ==============================================

# WORDFISH EXAMPLE

toks_irish <- tokens(data_corpus_irishbudget2010, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish)
tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(1, 2))
tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(1, 2), priors = c(Inf, Inf, 3, 1))
tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(1, 2), priors = c(Inf, Inf, 10, 1))
summary(tmod_wf)

textplot_scale1d(tmod_wf)
coef(tmod_wf)

# WORDCLOUD

new_df_trimmed_presidency <- rbind(dfm_trump_single_doc, dfm_biden_single_doc, dfm_obama_single_doc)
dfm_pres_trump_biden <- rbind(dfm_trump_single_doc, dfm_biden_single_doc)
dfm_pres_trump_obama <- rbind(dfm_trump_single_doc, dfm_obama_single_doc)
dfm_pres_biden_obama <- rbind(dfm_biden_single_doc, dfm_obama_single_doc)

dfm_pres_trump_biden <- n_dfm[n_dfm$presidency %in% c('Trump', 'Biden'), ]

textplot_wordcloud(dfm_trimmed, comparison = TRUE, max_words = 100, rotation = 0.15, color = c("blue", "red", 'green'))
textplot_wordcloud(dfm_pres_trump_biden, comparison = TRUE, max_words = 70, rotation = 0.15, color = c("blue", "red"))
textplot_wordcloud(dfm_pres_trump_obama, comparison = TRUE, max_words = 100, rotation = 0.15, color = c("blue", "red"))
textplot_wordcloud(dfm_pres_biden_obama, comparison = TRUE, max_words = 100, rotation = 0.15, color = c("blue", "red"))



textplot_wordcloud(n_dfm, comparison = TRUE, max_words = 100, rotation = 0.15, color = c("blue", "red", "#ee261b"))

dcinbox_nl %>%
  tail(10) %>%
  ggplot( aes(x=date, y=)) +
  geom_line() +
  geom_point()