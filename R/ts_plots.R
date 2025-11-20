



plot_ts <- function(all_data, station_name,par){

    pp <- ggplot(all_data, aes(x = .data[[colnames(all_data)[2]]], y = .data[[colnames(all_data)[3]]])) +
      geom_line(color = "#0072B2", linewidth = 0.6) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste0(station_name, " (", unique(all_data$station_no), ")"),
        subtitle = "Wasserstandsdaten",
        x = "Datum",
        y = paste(par, " [",colnames(all_data)[3],"]")
      ) +
      scale_x_datetime(
        date_breaks = "2 year",
        date_labels = "%Y"
      ) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        minor_breaks = NULL
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12)
      )

 #   ggsave(
 #     filename = file.path("your_file_path", paste0(station_name,"_",par, "_NRW.pdf")),
 #     plot = pp,
 #     width = 28,
 #     height = 21,
 #     units = "cm"
 #   )
return(pp)
}
