complaint_query <- function  (complaint_type){
    query_param <- soql() %>%
        soql_add_endpoint(api_endpoint) %>%
        soql_simple_filter("complaint_type",complaint_type)    
    query <- read.socrata(
        query_param,
        app_token = api_token
    )
    return(query)
}

DEP_filter <-function(data,startdate,enddate){
    output <- data %>% 
        filter(created_date >= as.Date(startdate)) %>% 
        filter(created_date <= as.Date(enddate),
               complaint_type == "Sewer") %>% 
        mutate(day = day(created_date),
               month = month(created_date),
               year = year(created_date),
               date = as.Date(paste0(year,"-",month,"-",day)))
    return(output)    
}

time_plotting_series <- function(data,title){
    plot<-ggplot(data %>% 
                     group_by(complaint_type,date) %>% 
                     count() %>% 
                     st_drop_geometry(), 
                 aes(date, n ,color = complaint_type)) +
        geom_line() +
        scale_color_viridis(discrete = T) +
        theme_minimal() +
        xlab(title) +
        scale_y_continuous(label=comma)
    return(plot)
}