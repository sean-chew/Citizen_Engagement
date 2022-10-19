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
# tm_shape(nyc_raster) +
# tm_rgb() +
final_plotting<-function(data,title,variable){
        plot<- tm_shape(data, unit = "imperial")+
            tm_fill(variable, 
                    palette = "PuBu", lwd = .01,alpha = .3,
                    style = "jenks") +
            tm_compass(size = .8) +
            tm_scale_bar(text.size = .5)+
            tm_layout(title = title,
                      title.color = "white",
                      title.size = 1,
                      legend.text.color = "white",
                      bg.color = "darkgrey", 
                      frame = FALSE)
    tmap_save(plot,paste0("../4_Outputs/2_FEMA_Charts/",str_replace_all(title, " ", ""),".pdf"))
}

zipcode_analysis <- function(zipcode,data,storm_points,title,buffer){
    processed <-  data %>%
        filter(ZIPCODE %in% zipcode) %>% 
        st_transform(crs = 2263)
    
    pluto <- map_pluto %>% 
        st_intersection(processed %>% st_buffer(buffer))
    
    parks <- parks %>% 
        st_transform(crs = 2263) %>% 
        st_intersection(processed %>% st_buffer(buffer))
    
    storm_contour_zip <- storm_points %>% 
        filter(incident_zip %in% zipcode) %>% 
        st_kde() %>% 
        st_get_contour(cont = c(10,20,30,40,50,60,70,80,90,100)) %>% 
        st_intersection()
    
    flooding_crop <- st_intersection(extreme_flood,processed %>% st_buffer(buffer))
    
    final_map <- tm_shape(parks) +
        tm_fill(col = "#99C2A2") + 
        tm_shape(pluto)+
        tm_borders("grey", alpha = .3, lwd = .5) +
        tm_shape(flooding_crop)+
        tm_fill(col = "#1D84B5")+
        tm_shape(storm_contour_zip,is.master = TRUE)+
        tm_fill(title = "Percentile","contlabel",
                palette = "PuBu", lwd = .01,alpha = .3) +
        tm_layout(title = title, legend.outside = TRUE,bg.color = "#132E32") +
        tm_scale_bar(position = c("left", "bottom"), width = 0.15) +
        tm_compass(position = c("left", "top"), size = 1)
    
    #print(paste0("../4_Outputs/2_FEMA_Charts/",str_replace_all(title, " ", ""),".pdf"))
    tmap_save(final_map,paste0("../4_Outputs/2_FEMA_Charts/",str_replace_all(title, " ", ""),".pdf"))
              
    return(final_map)
}
