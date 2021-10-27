# packages #
#####
require(shiny)
require(spotifyr)
require(tidyverse)
require(DT)
require(shinyjs)
require(data.table)

#####
# functions #
#####

set_up <- function(client_id, secret_id, playlist, username) {
  
  # sets up the the tokens for spotify requests and sets the playlist id and username #
  
  # tokens and username #
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret_id)
  access_token <- get_spotify_access_token()
  username <- username
  
} 

playlist_name_to_id <- function(name) {
  
  # takes playlist name and returns the id #
  
  # get playlists #
  user_playlists <- get_my_playlists()
  
  # get the info from playlists df #
  playlist_id <- user_playlists[user_playlists$name == name, 'id']
  
  return(playlist_id)
  
}

build_tracks <- function(username, playlist) {
  
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

build_artists <- function(data = tracks) {
  
  # builds df of artist info #
  
  # df of artist ids #
  artist_ids <- data %>% 
    select(track.artists.id.1, track.artists.name.1) %>% 
    unique() %>% 
    na.omit(track.artists.id.1)
  
  # need to load data in chunks of 50 due to request limit #
  artist_info <- data.frame()
  for (i in 0:(ceiling(nrow(artist_ids)/50) - 1)) {
    try(rm(out))
    if (i == 0) {
      
      # get artist info for first 50 #
      out <- get_artists(artist_ids$track.artists.id.1[1:50])
      
    } else if (i < ceiling(nrow(artist_ids)/50) - 1) {
      
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
    data.table::setnames(old, new)
  
  return(artist_info)
  
}

search_collection <- function(term, data = tracks) {
  
  # gets all songs in given playlist by artist given #
  
  # set up vars #
  term <- as.character(term)
  out <- data
  names <- data[,c(26:35,40)]
  
  # find tracks and make a coulmn to id #
  out$found <- apply(names, 1, function(x) {(any(grep(term, x, ignore.case = T)))})
  
  # select only rows where found is true, then trim columns #
  out <- filter(out, found == T) %>% 
    select(40, 26:30, 46)
  
  # remove extra columns #
  for (i in length(out):1) {
    if (sum(is.na(out[,i])) == nrow(out)) {
      out <- select(out, !(i))
    }
  }
  
  return(out)
}

followers_table <- function(genre = '', results = 50, follower_range = c(0,1000000000), decreasing = T, 
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
        select(name, followers.total, genre.1:genre.14) %>% 
        filter(followers.total >= follower_range[1] & followers.total <= follower_range[2]) %>% 
        arrange(desc(followers.total))
      
      # increasing #
    } else {
      
      out <- out %>% 
        select(name, followers.total, genre.1:genre.14) %>% 
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
  
  # remove extra columns # 
  for (i in length(out):1) {
    if (sum(is.na(out[,i])) == nrow(out)) {
      out <- select(out, !(i))
    }
  }
  
  # results param #
  rows_out <- min(as.numeric(results), nrow(out))
  
  out <- out[1:rows_out,]
  
  return(out)
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



#####
# load in data 
#####

set_up('141b22fb1aa8490a91bf0b65087f5f67','564d90b2fce64763aa06730ea408763e', 'data', 'corndoggen')

tracks <- build_tracks('corndoggen', 'data')

artists <- build_artists(tracks)

artist_char <- build_artists_char(artists)

all_unique_genres <- unique(c(artists$genre.1,artists$genre.2,artists$genre.3,artists$genre.4,artists$genre.5,artists$genre.6,
                              artists$genre.7,artists$genre.8,artists$genre.9,artists$genre.10,artists$genre.11,artists$genre.12,
                              artists$genre.13,artists$genre.14,artists$genre.15))

#####
# some shit #
#####

function (verb, url = NULL, config = list(), ..., body = NULL, 
          encode = c("multipart", "form", "json", 
                     "raw"), times = 3, pause_base = 1, pause_cap = 60, 
          pause_min = 1, handle = NULL, quiet = FALSE, terminate_on = NULL, 
          terminate_on_success = TRUE) 
{
  stopifnot(is.numeric(times), length(times) == 1L)
  stopifnot(is.numeric(pause_base), length(pause_base) == 1L)
  stopifnot(is.numeric(pause_cap), length(pause_cap) == 1L)
  stopifnot(is.numeric(terminate_on) || is.null(terminate_on))
  stopifnot(is.logical(terminate_on_success), length(terminate_on_success) == 
              1)
  hu <- handle_url(handle, url, ...)
  req <- request_build(verb, hu$url, body_config(body, match.arg(encode)), 
                       config, ...)
  resp <- tryCatch(request_perform(req, hu$handle$handle), 
                   error = function(e) e)
  i <- 1
  while (!retry_should_terminate(i, times, resp, terminate_on, 
                                 terminate_on_success)) {
    backoff_full_jitter(i, resp, pause_base, pause_cap, pause_min, 
                        quiet = quiet)
    i <- i + 1
    resp <- tryCatch(request_perform(req, hu$handle$handle), 
                     error = function(e) e)
  }
  if (inherits(resp, "error")) {
    stop(resp)
  }
  resp
}

function(user_id, limit = 20, offset = 0, authorization = get_spotify_authorization_code(), 
          include_meta_info = FALSE) 
{
  base_url <- "https://api.spotify.com/v1/users"
  url <- str_glue("{base_url}/{user_id}/playlists")
  params <- list(limit = limit, offset = offset)
  res <- RETRY("GET", url, query = params, config(token = authorization), 
               encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE)
  if (!include_meta_info) {
    res <- res$items
  }
  res
}

base_url <- "https://api.spotify.com/v1/users"
url <- str_glue("{base_url}/{user_id}/playlists")
params <- list(limit = 20, offset = 0)
authorization = get_spotify_authorization_code()

res <- httr::RETRY("GET", url, query = params, httr::config(token = authorization), encode = "json")

test <- httr::GET(url, httr::config(token = authorization))
test <- jsonlite::fromJSON(httr::content(test, as = 'text', encoding = 'UTF-8'), flatten = T)
view(test)

#####
# ui #
#####
# ui <- fluidPage(
# 
#    # Application title
#    titlePanel("Spotify Data"),
# 
#    sidebarLayout(
#       sidebarPanel(
#          # text input for artist name #
#          textInput(inputId = 'artist_name', 'All songs by artist in collection', value = 'Mac Miller'),
# 
#          # slider input for bins of hist #
#          # sliderInput('bins',label = 'test', min = 0, max = 100, value = 25)
#       ),
# 
#       # Show a plot of the generated distribution
#       mainPanel(
#         # plotOutput(outputId = 'distPlot'),
#         tableOutput(outputId = 'songsby')
#       )
#    )
# )
#####
# ui 2 #
#####

ui <- fluidPage(tabsetPanel(
  
  tabPanel("Follower Counts", 
           
           useShinyjs(),
           
           sidebarLayout(
             sidebarPanel(
               # follower range slider #
               sliderInput(inputId = 'follower_range', label = 'Follower Range',
                           min = 0, max = max(artists$followers.total), 
                           value = c(0,max(artists$followers.total)),
                           ticks = F,
                           step = 5000),
               
               # number of results #
               selectInput(inputId  = 'results', 
                           label = 'Number of results', 
                           choices = c(5,10,15,20,25,50,100),
                           selected = 50),
               
               # decreasing/increasing follower counts #
               checkboxInput(inputId = 'decreasing',
                             label = 'Sort followers decreasing?',
                             value = T),
    
               # checkbox to include genre filter #
               checkboxInput('include_genre', 'Include genre filter?'),
               
               # genre selection #
               uiOutput('genre')
               
             ),
             mainPanel(
               
               # table output #
               tableOutput(outputId = 'followers_table')
             )
           )
         ),
  
  tabPanel("Individual Artist Info", 
           sidebarLayout(
             sidebarPanel(
               
               # artist name input #
               textInput(inputId = 'radar_name1', 'artist name', value = 'Mac Miller'),
               
               # artist compartison checkbox #
               checkboxInput(inputId = 'artist_comparison', 'compare aritists?', value = F),
               
               # compartison artist name input #
               uiOutput(outputId = 'artist_comparison_ui')
               
             ),
             mainPanel(
               
               # radar output #
               plotOutput(outputId = 'radar')
             )
           )
          ),
  
  tabPanel("Collection Search", 
           
           # text input for artist name #
           textInput(inputId = 'term', 'Search Collection', value = 'Mac Miller'),
           
           # table output #
           tableOutput(outputId = 'search')
          )
        )
      )


#####
# server #
#####

server <- function(input, output) {
   
   # search results #
   output$search <- renderTable(search_collection(input$term))
   
   output$genre <- renderUI({
     
       autocomplete_input(id = 'genre', label = 'What genre?', options = all_unique_genres)
     
   })
   
   # show/hide genre selection #
   observe(
    shinyjs::toggle(id = 'genre', condition = input$include_genre)
   )
   
   output$followers_table <- renderTable(followers_table(genre = input$genre, 
                                                         include_gerne = input$include_genre,
                                                         follower_range = input$follower_range,
                                                         results = input$results,
                                                         decreasing = input$decreasing))
   
   # ui for artist comparison #
   output$artist_comparison_ui <- renderUI({
     
     textInput(inputId = 'radar_name2', 'comparison artist name', value = 'Kendrick Lamar')
     
   })
   
   # show/hide artist compartison #
   observe(shinyjs::toggle(id = 'artist_comparison_ui', condition = input$artist_comparison))
   
   # render radar charts #
   output$radar <- renderPlot({
     
     if (!input$artist_comparison) {
       
       make_radar(input$radar_name1)
       
     } else {
         
       radar_names <- c(input$radar_name1, input$radar_name2)
       make_radar(radar_names)
       
     }
     
     })
   

   
   {
     # output$distPlot <- renderPlot({
     #    # generate bins based on input$bins from ui.R
     #    x    <- faithful[, 2] 
     #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
     #    
     #    # draw the histogram with the specified number of bins
     #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
     # })
   }
   
   
}

#####
# Run the application 
#####
shinyApp(ui = ui, server = server)

