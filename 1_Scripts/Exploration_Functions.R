descriptors <- function(department) {
    descriptors <- query %>% 
        filter(agency == department) %>% 
        pull(descriptor) %>% 
        unique() %>% 
        as.data.frame()
    return(descriptors)
}

# Function for filtering complaints depending on what years and complaints we will 
# later want to visualize
complaint_filter <- function(complaints,complaint_types,level,years){
    filtered <- complaints %>%  
        st_drop_geometry %>% 
        filter(complaint_type %in% complaint_types,
               year %in% years) %>% 
        group_by(ct2010, boro_name) %>% 
        summarise(n = sum(n))
    return(filtered)
}

# Function for creating choropleth maps depending on the filtered data
complaint_mapping <- function(shapefile,filtered,output_type,years,complaints){
    
    if(length(complaints) == length(c("Air Quality","Lead","Water Conservation","Industrial Waste",
                                      "Sewer","Water System","Asbestos","Hazardous Materials"))){
        complaints = "All Complaints"
    } else {
        complaints = glue_collapse(complaints,", ")
    }
    ct <- shapefile %>% 
        left_join(filtered,by= c("ct2010","boro_name")) 
    map <- tm_shape(ct) +
        tm_fill("n",title="Complaints",style="quantile", palette = "viridis", lwd = .01) +
        tm_layout(main.title = paste0("NYC Complaints from \n",complaints),
                  main.title.position = "center",
                  main.title.size = 1,
                  main.title.fontface = 2
        )
    if(output_type == "map"){
        return(map)
    }
    else if(output_type == "shape"){
        return(ct)
    }
    
    
}

popjoin <- function(ct,pops){
    census_shapes <- ct %>% 
        left_join(pops, by = c("boro_name", "ctlabel")) %>% 
        mutate(measure = na_if(measure,0),
               measure = na_if(measure,2),
               percapita = n/measure)
    return(census_shapes)
}

plotting_percapita <- function(ct,complaints){
    map <- tm_shape(ct) +
        tm_fill("percapita",title="Complaints",style="quantile", palette = "viridis", lwd = .01,n =10) +
        tm_layout(main.title = paste0("NYC Complaints from \n",complaints),
                  main.title.position = "center",
                  main.title.size = 1,
                  main.title.fontface = 2)
    return(map)
}

plotting_perDEP <- function(ct,complaints){
    map <- tm_shape(ct) +
        tm_fill("DEP_prop",title="Complaints",style="quantile", palette = "viridis", lwd = .01) +
        tm_layout(main.title = paste0("NYC Complaints from \n",complaints),
                  main.title.position = "center",
                  main.title.size = 1,
                  main.title.fontface = 2,
                  legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f"), " %")))
    return(map)
}

join_shapes <- function(littleshape){
    DEP_Normalized <- ct %>% 
        inner_join(ct_complaints_all_1 %>% st_drop_geometry,by = c("ct2010","boro_name")) %>% 
        rename(total = n) %>% 
        inner_join(littleshape, by =  c("ct2010","boro_name")) %>% 
        mutate(DEP_prop = n/total*100)
    return(DEP_Normalized)
}

plotComplaints <- function(data,time_frame,n,complaint_type) {
    plot <- ggplot(data, aes(time_frame, n )) + #,fill = complaint_type
        geom_area() +
        scale_fill_viridis(discrete = T) +
        theme_minimal()  +
        xlab("2010 to Present") +
        scale_y_continuous(label=comma) +
        ggtitle(deparse(substitute(data)))
    return(plot)
    
}
plotline <- function(data,time_frame,n,complaint_type) {
    plot <- ggplot(data, aes_string(time_frame, n ,group = complaint_type, color = complaint_type)) +
        geom_line() +
        scale_color_viridis(discrete = T) +
        theme_minimal()  +
        xlab("2010 to Present") +
        scale_y_continuous(label=comma)+
        ggtitle(deparse(substitute(data)))
    return(plot)
}

save_plots <- function(plot){
    ggsave(paste0("../4_Outputs/",deparse(substitute(plot)),".jpg"),
           plot = plot,width = 10, height = 4)
}
