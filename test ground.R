require(shiny) # for web app 
require(spotifyr) # spotify data 
require(dqshiny) # autocompleteInput
require(tidyverse) # general stuff
require(data.table) # some general stuff
require(DT) # nice shiny tables (can't get it to work lol)
require(ggpubr) # ggarrange
require(fmsb) # radar charts 


#####
# functions #
#####

search_collection_test <- function(term, data = tracks) {
  
  # gets all songs in given playlist by artist given #
  
  # set up vars #
  term <- as.character(term)
  out <- data
  names <- data[,c(26:35,40)]
  
  # find tracks and make a coulmn to id #
  out$found <- apply(names, 1, function(x) {(any(grep(term, x, ignore.case = T)))})
  
  # select only rows where found is true, then trim columns #
  out <- filter(out, found == T) %>% 
    select(40, 24:30, 46)
  
  # remove extra columns #
  for (i in length(out):1) {
    if (sum(is.na(out[,i])) == nrow(out)) {
      out <- select(out, !(i))
    }
  }
  
  return(out)
}

playlist_name_to_id_test <- function(name, userId) {
  
  # takes playlist name and returns the id #
  
  # get playlists #
  user_playlists <- get_user_playlists(userId)
  
  # get the info from playlists df #
  playlist_id <- user_playlists[user_playlists$name == name, 'id']
  
  return(playlist_id)
  
}

set_up_test <- function(client_id, secret_id, playlist, username) {
  
  # sets up the the tokens for spotify requests and sets the playlist id and username #
  
  # tokens and username #
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret_id)
  access_token <- get_spotify_access_token()
  username <- username
  
} 

build_tracks_test <- function(username, playlist) {
  
  # loads in data from playlist and creates and cleans mulitple dataframes for use #
  
  # playlist name to id #
  playlist_id <- playlist_name_to_id(playlist)
  
  # builds the df with all tracks from playlist #
  tracks <- get_playlist_audio_features(username, playlist_id) %>% 
    select(!c(playlist_id, playlist_img, playlist_owner_name, playlist_owner_id, analysis_url, is_local,
              added_by.href, added_by.id, added_by.type, added_by.id, added_by.external_urls.spotify,
              added_by.uri, track.available_markets, track.episode, track.href, track.href, track.preview_url,
              track.track, track.type, track.album.artists, track.album.available_markets, track.album.href,
              track.album.images, track.album.type, track.album.external_urls.spotify, track.external_ids.isrc,
              track.external_urls.spotify, video_thumbnail.url, primary_color)) %>%
    na.omit(track.name) %>%
    unnest_wider(track.artists, names_sep = '.') %>%
    unnest_wider(track.artists.id, names_sep = '.') %>%
    unnest_wider(track.artists.name, names_sep = '.') %>%
    select(!c(track.artists.type, track.artists.href, track.artists.uri, track.artists.external_urls.spotify))
  
  return(tracks)
}

build_artists_test <- function(tracks = tracks) {
  
  # builds df of artist info #
  
  # df of artist ids #
  artist_ids <- tracks %>% 
    select(track.artists.id.1, track.artists.name.1) %>% 
    unique() %>% 
    na.omit(track.artists.id.1)
  
  # need to load data in chunks of 50 due to request limit #
  artist_info <- data.frame()
  for (i in 0:(ceiling(nrow(artist_ids)/50)-1)) {
    try(rm(out))
    if (i == 0) {
      
      # get artist info for first 50 #
      out <- get_artists(artist_ids$track.artists.id.1[1:50])
      
    } else if (i < ceiling(nrow(artist_ids)/50)-1){
      
      # indexes #
      x = i * 50 + 1;
      y = i * 50 + 50;
      
      # get artist info for index #
      out <- get_artists(artist_ids$track.artists.id.1[x:y])
      
    } else {
      
      # indexes #
      z = i*50 + 1;
      q = i*50 + nrow(artist_ids) - i*50;
      
      # get artist info for index #
      out <- get_artists(artist_ids$track.artists.id.1[z:q])
      
    }
    
    # appends cycle output to artist_info #
    artist_info <- rbind(artist_info,out)
    
  }
  
  # finds max genre depth #
  uh <- c()
  for (i in 1:nrow(artist_info)) uh[i] <- length(artist_info$genres[[i]])
  genre_depth = max(uh)
  
  # creates lists used to rename columns #
  old <- c()
  for (i in 1:genre_depth) old[i] <- paste0('...',i) 
  
  new <- c()
  for (i in 1:genre_depth) new[i] <- paste0('genre.',i)
  
  # clean up df #
  artist_info <- artist_info %>% 
    select(genres, id, name, popularity, uri, followers.total) %>%
    unnest_wider(genres) %>% 
    setnames(old, new)
  
  rm(genre_depth, new, old, uh)
  
  return(artist_info)
  
}

followers_table_test <- function(genre = '', results = 50, follower_range = c(0,1000000000), decreasing = T, 
                                 include_gerne = F) {
  
  # shows list of artists and follower counts with options for display #
  
  # setup output #
  out <- artists
  
  # genre filter #
  if (genre != '') {
    
    genre <- as.character(genre)
    names <- artists[,1:15]
    
    out$found <- apply(names, 1, function(x) {(any(grep(genre, x, ignore.case = T)))})
    
    out <- filter(out, found == T)
    
  } 
  
  # with genres listed #
  if (include_gerne) {
    
    # decreasing follower count #
    if (decreasing) {
      
      out <- out %>% 
        select(name, followers.total, genre.1:genre.15) %>% 
        filter(followers.total >= follower_range[1] & followers.total <= follower_range[2]) %>% 
        arrange(desc(followers.total))
      
      # increasing #
    } else {
      
      out <- out %>% 
        select(name, followers.total, genre.1:genre.15) %>% 
        filter(followers.total >= follower_range[1] & followers.total <= follower_range[2]) %>% 
        arrange(followers.total)
      
    }
    
    
    # without genres listed # 
    
  } else {
    
    # decreasing follower count #
    if (decreasing) {
      
      out <- out %>% 
        select(name, followers.total) %>% 
        filter(followers.total >= follower_range[1] & followers.total <= follower_range[2]) %>% 
        arrange(desc(followers.total)) 
      
      # increasing #
    } else {
      
      out <- out %>% 
        select(name, followers.total) %>% 
        filter(followers.total >= follower_range[1] & followers.total <= follower_range[2]) %>% 
        arrange((followers.total)) 
      
    }
    
  }
  
  # results param #
  results <- min(results, nrow(out))
  out <- out[1:results,]
  
  
  # remove extra columns # 
  for (i in length(out):1) {
    if (sum(is.na(out[,i])) == nrow(out)) {
      out <- select(out, !(i))
    }
  }
  
  return(out)
}

#####
# load in data 
#####

#####
# builds tracks df #
#####

Sys.setenv(SPOTIFY_CLIENT_ID = '141b22fb1aa8490a91bf0b65087f5f67')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '564d90b2fce64763aa06730ea408763e')
access_token <- get_spotify_access_token()
data <- '0480aOWQNkfLEIJjffyXDr'

tracks <- get_playlist_audio_features('corndoggen', data) 
tracks <- tracks[!duplicated(tracks$track.name),] 
tracks <- unnest_wider(tracks, track.artists, names_sep = '.') %>% 
  unnest_wider(track.artists.id, names_sep = '.') %>% 
  unnest_wider(track.artists.name, names_sep = '.') %>% 
  select(!c(track.artists.type, playlist_id, playlist_img, playlist_owner_name, playlist_owner_id, analysis_url,
            is_local, added_by.href, added_by.id, added_by.type, added_by.id, added_by.external_urls.spotify, 
            added_by.uri, track.artists.href, track.artists.uri, track.artists.external_urls.spotify, 
            track.available_markets, track.episode, track.href, track.href, track.preview_url, track.track, 
            track.type, track.album.artists, track.album.available_markets, track.album.href, track.album.images,
            track.album.type, track.album.external_urls.spotify, track.external_ids.isrc, 
            track.external_urls.spotify, video_thumbnail.url)) 


#####
# builds artist info df #
#####
artist_ids <- tracks %>% 
  select(track.artists.id.1, track.artists.name.1) %>% 
  unique() %>% 
  na.omit(track.artists.id.1)


# need to load data in chunks of 50 due to request limit #
artist_info <- data.frame()
for (i in 0:(ceiling(nrow(artist_ids)/50)-1)) {
  try(rm(out))
  if (i == 0) {
    
    # get artist info for first 50 #
    out <- get_artists(artist_ids$track.artists.id.1[1:50])
    
  } else if (i < ceiling(nrow(artist_ids)/50)-1){
    
    # indexes #
    x = i * 50 + 1;
    y = i * 50 + 50;
    
    # get artist info for index #
    out <- get_artists(artist_ids$track.artists.id.1[x:y])
    
  } else {
    
    # indexes #
    z = i*50 + 1;
    q = i*50 + nrow(artist_ids) - i*50;
    
    # get artist info for index #
    out <- get_artists(artist_ids$track.artists.id.1[z:q])
    
  }
  
  # appends cycle output to artist_info #
  artist_info <- rbind(artist_info,out)
  
}

uh <- c()
for (i in 1:nrow(artist_info)) uh[i] <- length(artist_info$genres[[i]])
genre_depth = max(uh)

old <- c()
for (i in 1:genre_depth) old[i] <- paste0('...',i) 

new <- c()
for (i in 1:genre_depth) new[i] <- paste0('genre.',i)

test <- artist_info %>% 
  select(genres, id, name, popularity, uri, followers.total) %>%
  unnest_wider(genres) %>% 
  setnames(old, new)

rm(genre_depth, new, old, uh)

view(test)

uh <- c()
for (i in 1:nrow(artist_info)) uh[i] <- length(artist_info$genres[[i]])
max(uh)

# checks my counting (indexing) #
for (i in 0:(ceiling(nrow(artist_ids)/50)-1)) {
  try(rm(out))
  if (i == 0) {
    print(ceiling(nrow(artist_ids)/50)-1)
    print(i)
  } else if (i < ceiling(nrow(artist_ids)/50)-1){
    print(i)
    x = i * 50 + 1;
    y = i * 50 + 50;
  } else {
    print(i)
    z = i*50;
    q = i*50 + nrow(artist_ids) - i*50;
    print(z);
    print(q)
  }
}


# checks which ids aren't in final dataframe # 
notin <- artist_ids[!(artist_ids$track.artists.id.1 %in% artist_info$id),]
view(notin)

#####
# random shit #
#####



uh <- data.frame(table(all_genres)) %>%
  filter(Freq >= 3) %>%
  arrange(desc(Freq))
view(uh)


all_genres <- c(artists$genre.1,artists$genre.2,artists$genre.3,artists$genre.4,artists$genre.5,artists$genre.6,
                artists$genre.7,artists$genre.8,artists$genre.9,artists$genre.10,artists$genre.11,artists$genre.12,
                artists$genre.13,artists$genre.14,artists$genre.15)

all_unique_genres <- unique(c(artists$genre.1,artists$genre.2,artists$genre.3,artists$genre.4,artists$genre.5,artists$genre.6,
                              artists$genre.7,artists$genre.8,artists$genre.9,artists$genre.10,artists$genre.11,artists$genre.12,
                              artists$genre.13,artists$genre.14,artists$genre.15))


#####
# build data for artists char #
#####

get_artists_char <- function(x) {
  
  temp <- tryCatch({
    artist_cat <- get_artist_audio_features(x['id'])
    
    temp <- lapply(artist_cat[,c(9,10,14:18)], median)
    
    temp$id <- as.character(x['id'])
    temp$name <- as.character(x['name'])
    
    as.data.frame(temp, stringsAsFactors = F)
  },
  error = function(e) return(NA))
  
  return(temp)
}


build_artists_char <- function(artists) {
  
  out <- data.frame(danceability = rep(NA, nrow(artists)),
                    energy = rep(NA, nrow(artists)),
                    speechiness = rep(NA, nrow(artists)),
                    acoutsticness = rep(NA, nrow(artists)),
                    instrumentalness = rep(NA, nrow(artists)),
                    liveness = rep(NA, nrow(artists)),
                    valence = rep(NA, nrow(artists)),
                    id = rep('', nrow(artists)),
                    name = rep('', nrow(artists)),
                    stringsAsFactors = F)
  
  for (i in 1:nrow(artists)) {
    out[i,] <- get_artists_char(artists[i,])
  }
  
  # clean up df #
  out <- na.omit(out)
  
  # create min and max row for radar charts #
  maxes <- apply(out[,1:7], 2, max, na.rm = T)
  mins <- apply(out[,1:7], 2, min, na.rm = T)

  out <- rbind(maxes, mins, out)
  out[1,8:9] <- 'max'
  out[2,8:9] <- 'min'
  
  # creates median row for radar charts #
  median <- apply(out[1:7], 2, median, na.rm = T)
  out <- rbind(out[1:2,], median, out[3:nrow(out),])
  out[3, c('name','id')] <- 'median'
  
  row.names(out) <- 1:nrow(out)
  
  return(out)
  
}

wuh <- build_artists_char(head(artists, 10))
view(wuh)

#####
# produce radial charts #
#####

uh <- apply(artist_char[,1:7], 2, max, na.rm = T)
uhh <- apply(artist_char[,1:7], 2, min, na.rm = T)

test <- rbind(uh, uhh, na.omit(artist_char))
test[1,8:9] <- 'max'
test[2,8:9] <- 'min'

tester <- apply(test[1:7], 2, median, na.rm = T)
test <- rbind(test[1:2,], tester, test[3:nrow(test),])
test[3, 8:9] <- 'median'
# view(test)

{
x = 69

colors_border = c(rgb(0.7,0.2,0.1,0.9), rgb(0.1,0.5,0.2,0.2))
colors_in = c(rgb(0.7,0.2,0.1,0.4), rgb(0.1,0.5,0.2,0.4))
colors_words = c(rgb(0.7,0.2,0.1,1), rgb(0.1,0.5,0.2,1))
  
radarchart(test[c(1,2,x,3), 1:7], 
           title = as.character(test[x,'name']),
           pty = 32, pcol = colors_border, plty = 1, plwd = 2, pfcol = colors_in, 
           cglty = 1, cglwd = 1, cglcol = 'grey')

legend(x = 0.6, y = 1.25, legend = test[c(x, 3),'name'], bty = "n", pch = 20, col = colors_words, 
       text.col = rgb(0,0,0,1), cex = 1, pt.cex = 3)

}

make_radar <- function(name, char = artist_char) {
  
  # colors for graphs #
  colors_border = c(rgb(0.7,0.2,0.1,0.9), rgb(0.1,0.5,0.2,0.2))
  colors_in = c(rgb(0.7,0.2,0.1,0.4), rgb(0.1,0.5,0.2,0.4))
  colors_words = c(rgb(0.7,0.2,0.1,1), rgb(0.1,0.5,0.2,1))
  
  # set up data for radar charts #
  maxes <- apply(char[,1:7], 2, max, na.rm = T)
  mins <- apply(char[,1:7], 2, min, na.rm = T)
  
  data <- rbind(maxes, mins, na.omit(char))
  data[2,8:9] <- 'max'
  data[2,8:9] <- 'min'
  
  data[(nrow(data) + 1),1:7] <- apply(data[1:7], 2, median, na.rm = T)
  data[nrow(data), 'name'] <- 'median'
  
  # assigns index number from artist name #
  name_index <- c()
  for (i in 1:length(name)) {name_index[i] <- as.numeric(rownames(data[data$name == name[i],]))}
  
  # make chart #
  if (length(name) == 1) {
      
    radarchart(data[c(1, 2, name_index[1], 5), 1:7], 
               title = as.character(data[name_index[1], 'name']),
               pty = 32, pcol = colors_border, plty = 1, plwd = 2, pfcol = colors_in, 
               cglty = 1, cglwd = 1, cglcol = 'grey')
    
    legend(x = 0.6, y = 1.25, legend = data[c(name_index[1], 5), 'name'], bty = "n", pch = 20, col = colors_words, 
           text.col = rgb(0,0,0,1), cex = 1, pt.cex = 3)
    
  } else if (length(name) > 1) {
    
    colors_border = c(rgb(0.7,0.2,0.1,0.9), rgb(0.1,0.5,0.2,0.9))

    radarchart(data[c(1, 2, name_index), 1:7], 
               title = paste(name, sep = ' vs '),
               pty = 32, pcol = colors_border, plty = 1, plwd = 2, pfcol = colors_in, 
               cglty = 1, cglwd = 1, cglcol = 'grey')
    
    legend(x = 0.6, y = 1.25, legend = data[name_index, 'name'], bty = "n", pch = 20, col = colors_words, 
           text.col = rgb(0,0,0,1), cex = 1, pt.cex = 3)
    
  }
  
}

make_radar('Kendrick Lamar')
make_radar(c('Kendrick Lamar','Mac Miller'))

#####
# distribution of characteristics #
#####


test <- get_artist_audio_features(artists[2,'id'])
test2 <- get_artist_audio_features(artists[3, 'id'])
test3 <- get_artist_audio_features(artists[artists$name == 'Kendrick Lamar', 'id'])

tester <- bind_rows(test,test2)

ggplot(tester, aes(x = valence, fill = artist_name)) +
  geom_density(alpha = .5)

uh <- function() {
  
  val <- ggplot(tester, aes(x = valence, fill = artist_name)) +
    geom_density(alpha = .5)
  
  dance <- ggplot(tester, aes(x = danceability, fill = artist_name)) +
    geom_density(alpha = .5)
  
  out <- ggarrange(val, dance, 
                   nrow = 2, common.legend = T)
}

yuh <- uh()
yuh


{
test_aco <- ggplot(test, aes(x = acousticness)) +
  geom_density()

test_speech <- ggplot(test, aes(x = speechiness)) +
  geom_density()

test_energy <- ggplot(test, aes(x = energy)) +
  geom_density()

test_val <- ggplot(test, aes(x = valence)) +
  geom_density()

test_dance <- ggplot(test, aes(x = danceability)) +
  geom_density()

tests_live <- ggplot(test, aes(x = liveness)) +
  geom_density()

test_inst <- ggplot(test, aes(x = instrumentalness)) +
  geom_density()

ggarrange(test_speech, test_aco, test_dance, test_energy, test_val, test_inst, tests_live,
          ncol = 3, nrow = 3)
}

{
test2_aco <- ggplot(test2, aes(x = acousticness)) +
  geom_density()

test2_speech <- ggplot(test2, aes(x = speechiness)) +
  geom_density()

test2_energy <- ggplot(test2, aes(x = energy)) +
  geom_density()

test2_val <- ggplot(test2, aes(x = valence)) +
  geom_density()

test2_dance <- ggplot(test2, aes(x = danceability)) +
  geom_density()

test2s_live <- ggplot(test2, aes(x = liveness)) +
  geom_density()

test2_inst <- ggplot(test2, aes(x = instrumentalness)) +
  geom_density()

ggarrange(test2_speech, test2_aco, test2_dance, test2_energy, test2_val, test2_inst, test2s_live,
          ncol = 3, nrow = 3)
}


#####
# radar charts #
#####

{
um <- lapply(test[, c(9,10,14:18)], median)

um <- as.data.frame(um)

um <- rbind(rep(1,7), rep(0,7), um)

radarchart(um, title = 'borns')
}

{
um1 <- lapply(test2[,c(9,10,14:18)], median)

um1 <- as.data.frame(um1)

um1 <- rbind(rep(1,7), rep(0,7), um1)

radarchart(um1, title = 'bishop')
}

{
um2 <- lapply(test3[, c(9,10,14:18)], median)

um2 <- as.data.frame(um2)

um2 <- rbind(rep(1,7), rep(0,7), um2)

radarchart(um2, title = 'kendrick')
}

