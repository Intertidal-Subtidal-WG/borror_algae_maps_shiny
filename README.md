# Shiny interface to the Borror SML canopy maps


This is a shiny app to show change in shallow subtidal habitats through time to pair with the following manuscript:

Byrnes, Jarrett E. K., Andrea Brown, Kate Sheridan, Tianna Peller, Jake Lawlor, Julien Beaulieu, Jenny Muñoz-Z, et al. 2022. “Notes from the Past Show How Local Variability Can Stymie Urchins and the Rise of the Reds in the Gulf of Maine.” EcoEvoRxiv. May 10. doi:10.32942/osf.io/u6exy.

The app is currently available at https://shiny.umb.edu/shiny/users/jarrett.byrnes/borror_algae_maps_shiny/

To run the app, you will need to install [R](http://r-project.org) and [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/).

Open the file `borror_algae_maps_shiny.Rproj` which should open RStudio. From here, open the file `app.R`.

If you do not have the requisite libraries installed, run the following at the console prompt:

```
install.packages(c("shiny", "leaflet", "sf", "dplyr", "glue", "here", "remotes"))
```

After this, you should be able to click the "Run App" button and enjoy.
