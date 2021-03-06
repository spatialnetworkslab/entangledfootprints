Design base map
================
Qingqing Chen
Last compiled date: 17 September, 2021

## Prepare datasets

``` r
# sg planning areas
sg_planing_areas <- read_sf(here("data/raw_data/sg-subzone/MP14_SUBZONE_NO_SEA_PL.shp")) %>%
  st_make_valid() %>%
  st_transform(crs = 3414) %>%
  group_by(REGION_N, PLN_AREA_N, SUBZONE_N) %>%
  dplyr::summarise() %>%
  ungroup()

# sg boundary
if(file.exists(here("data/derived_data/sg_boundary.rds"))){
  sg_boundary <- readRDS(here("data/derived_data/sg_boundary.rds"))
}else{
  sg_boundary <- sg_planing_areas %>% summarise()
  saveRDS(sg_boundary, file = here("data/derived_data/sg_boundary.rds"))
}

# get sg bounding box
sg_bbox <- getbb("singapore") %>% as_tibble()
if(file.exists(here("data/derived_data/streets.rds"))){
  streets <- readRDS(here("data/derived_data/streets.rds"))
}else{
  streets <- opq(bbox = c(sg_bbox$min[1], sg_bbox$min[2], sg_bbox$max[1], sg_bbox$max[2])) %>%
    add_osm_feature(key = "highway", value = c("primary", "trunk")) %>%
    osmdata_sf()
  streets <- streets$osm_lines %>%
    filter(!is.na(name)) %>%
    st_simplify(dTolerance = 0.0001) %>%
    st_transform(crs = 3414) %>%
    st_join(., sg_planing_areas, largest = T) %>%
    filter(!is.na(REGION_N))
  saveRDS(streets, file = here("data/derived_data/streets.rds"))
}

if(file.exists(here("data/derived_data/area_centers.rds"))){
  area_centers <- readRDS(here("data/derived_data/area_centers.rds"))
}else{
  # reference area
  area <- sg_planing_areas %>%
    filter(SUBZONE_N %in% c("NATIONAL UNIVERSITY OF S'PORE", "SENTOSA", "EAST COAST", "WOODLANDS", "CENTRAL WATER CATCHMENT", "JURONG GATEWAY", "CITY HALL",  "CHANGI AIRPORT", "TAMPINES EAST", "SERANGOON CENTRAL")) %>%  # NORTHSHORE
    mutate(SUBZONE_N = map_chr(SUBZONE_N, stringr::str_to_title))

  # reference area centers
  area_centers <- area %>%
    st_centroid() %>% rownames_to_column(var = "id") %>%
    mutate(label = paste0(id, ". ", SUBZONE_N)) %>%
    mutate(label = factor(label, levels = label))
  saveRDS(area_centers, file = here("data/derived_data/area_centers.rds"))
}

# hatched reference areas
if(file.exists(here("data/derived_data/area_hatched.rds"))){
  area_hatched <- readRDS(here("data/derived_data/area_hatched.rds"))
}else{
  area_hatched <- hatchedLayer(area, pattern = "right2left", mode = "sfc", density = 7)
  saveRDS(area_hatched, file = here("data/derived_data/area_hatched.rds"))
}
```

## Prepare image

``` r
tune_img <- function(img_nm, img_title, geom_size){
  base_path <- "data/photos/"
  img <- image_read(here(paste0(base_path, img_nm))) %>%
  image_resize(geometry = geom_size)
  ggdraw() +
    draw_image(img, x = 0.9, y = 0.9, hjust = 1, vjust = 1, height = 0.88) +
    draw_plot_label(img_title, fontface = "plain", x = 0.0, y = 0.99, hjust = 0, vjust = 1, size = 10)
}

img1 <- tune_img(img_nm = "cityhall.jpeg", img_title = "1. City Hall", geom_size = "400x550")
img2 <- tune_img(img_nm = "flickr-eastcoast.jpg", img_title = "2. East Coast", geom_size = "400x550")
img3 <- tune_img(img_nm = "flickr-nus.jpg", img_title = "3. National University of Singapore", geom_size = "400x550")
img4 <- tune_img(img_nm = "flickr-sentosa.jpg", img_title = "4. Sentosa", geom_size = "400x550")
img5 <- tune_img(img_nm = "flickr-changiairport.jpg", img_title = "5. Changi Airport", geom_size = "400x550")
img6 <- tune_img(img_nm = "flickr-tampines.jpg", img_title = "6. Tampines", geom_size = "400x550")
img7 <- tune_img(img_nm = "flickr-central-water-catchment.jpg", img_title = "7. Central Water Catchment", geom_size = "400x550")
img8 <- tune_img(img_nm = "flickr-woodlands.jpg", img_title = "8. Woodlands", geom_size = "400x550")
# img8 <- tune_img(img_nm = "flickr-northshore.jpg", img_title = "8. Northshore", geom_size = "400x550")
img9 <- tune_img(img_nm = "flickr-serangoon.jpg", img_title = "9. Serangoon Centre", geom_size = "400x550")
img10 <- tune_img(img_nm = "flickr-jurongeast.jpg", img_title = "10. Jurong Gateway", geom_size = "400x550")
```

## Draw base map

``` r
base_map <- tm_shape(sg_boundary) +
  tm_borders(col = "grey") +
  tm_shape(streets) +
  tm_lines(col = rgb(80, 80, 80, maxColorValue = 255), lwd = 1, alpha = 1) +
  tm_shape(area_hatched) +
  tm_lines(col = rgb(80, 110, 120, maxColorValue = 255)) +
  tm_shape(area_centers) +
  tm_bubbles(col = rgb(80, 80, 80, maxColorValue = 255), size = 1) +
  tm_text(text = "id", col = "white", size = 1) +
  tm_layout(frame = F)

grid.newpage()
pushViewport(viewport(layout = grid.layout(ncol = 3, nrow = 5, widths = c(3.4, 0.9, 0.9))))
print(base_map, vp = viewport(layout.pos.col = 1))
grid.text("Labeled places are reference areas", x = 0.5, y = 0.1, just = c(1, 1), gp = gpar(fontsize = 12, fontface = "italic"), vp = viewport(layout.pos.col = 1))
grid.text("Singapore map", x = 0.35, y = 0.06, just = c(1, 1), gp = gpar(fontsize = 12, fontface = "italic"), vp = viewport(layout.pos.col = 1))
print(img1, vp = viewport(layout.pos.col = 2, layout.pos.row = 1))
print(img2, vp = viewport(layout.pos.col = 2, layout.pos.row = 2))
print(img3, vp = viewport(layout.pos.col = 2, layout.pos.row = 3))
print(img4, vp = viewport(layout.pos.col = 2, layout.pos.row = 4))
print(img5, vp = viewport(layout.pos.col = 2, layout.pos.row = 5))
print(img6, vp = viewport(layout.pos.col = 3, layout.pos.row = 1))
print(img7, vp = viewport(layout.pos.col = 3, layout.pos.row = 2))
print(img8, vp = viewport(layout.pos.col = 3, layout.pos.row = 3))
print(img9, vp = viewport(layout.pos.col = 3, layout.pos.row = 4))
print(img10, vp = viewport(layout.pos.col =3, layout.pos.row = 5))
```

<img src="01-design-base-map_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

## Photo links

-   [1. City Hall photography by the author]()
-   [2. East Coast photography by Tamaki
    Hayashi](https://www.flickr.com/photos/hayashi28/16187474/in/photolist-2qXYo-qCvt7K-26wSwcJ-kdaVu7-27yncys-2eMRcjE-w4hvEx-24T2eJh-KuRxG5-pcnWhA-21cq2Ng-YXSYvY-6aXnGB-R75jxc-6gLWAK-6g9xTZ-6gLWAP-27CLs4P-fTqkGF-bAk7oS-2g4jTCz-2g4jYpG-R75kAp-YnLmvG-srHoHY-2eMQQib-2dLuWHw-V31HLx-2dtEcc6-2eMQMdy-spAc1Q-2eMQMC1-2dLuYfQ-GYKYLE-9evMBH-abgqEr-9ibhpX-8fm2wr-4FwJcy-5X6RJu-W9WWcK-arJG48-9eyTM5-9vbbes-6WAMiU-qCvVRr-gJBndV-B1aft-4CfEFn-dmJHhU)
-   [3. National University of Singapore photography by Melvin
    Yap](https://www.flickr.com/photos/mjmyap/12746332024/in/photolist-kqmhrf-kd1VQp-kd1WeR-56JJ3-6ajFyw)
-   [4. Sentosa photography by William
    Cho](https://www.flickr.com/photos/adforce1/5559170576/in/photolist-9tfcJy-yENBCs-a6dsbx-gvdAZA-gvfCpz-gvfh59-gv9Jdn-gvaVma-gvabFJ-gveNCH-gvafdy-gvakNP-gvdM3G-gv9wGL-gvf1rT-gvbAxj-gveNxp-gva7Er-gv9JAf-gvaaaB-gvbE4M-8hCRLa-gvbLuU-gvbHoo-gveK3D-gvcjEn-gveE5x-gveSRK-gveSk5-gvbcUT-gvbRz3-R8vPx6-gwRKmM-gvfd9v-gvcoiz-gvc1Td-gvccSe-gvbHgD-gv9XHD-gvbDRU-gv6XX1-gvdxZs-ejZX6j-gv84r6-gwR99w-gveGmV-gwSaTH-gwSjC4-gwRsrp-gv7RtD)
-   [5. Changi Airport photography by Geoff
    Whalan](https://www.flickr.com/photos/geoffwhalan/48355675096/in/photolist-2gF2tw9-2gF1XF7-2jdxvdU-2jjBSA9-2gHKZTv-2gPxibT-2gPwfCN-2gPwnKD-2fs2UAE-2e8NpV8-2ie9BdV-2jjzcQe-2gEY8uh-2iebWic-2jxGTU4-2hbfZA9-2h1kTU3-2jjz6rh-2jjyQST-2jbMbxg-24UyAXV-2g2i2TN-2e8NpUB-2e8NpUg-2jxGSRT-RLiBue-2jFRJqy-2jjBQLT-2iCLYF9-2jxCwhk-2hq6rfz-2hAJ5z7-2hAJ5X1-2hAFeDC-2hAJ5QH-2hAJ5Lu-2hAJYz5-2hHcTuw-2hAJ5pn-2gZQN1S-2jxGTBa-2gpdtGz-2gpcFV9-2gpd5KB-2gpciRt-2estN52-2gpcj5p-2gpdtCB-2gpd5RU-2gZR7LY)
-   [6. Tampines photography by Dickson
    Phua](https://www.flickr.com/photos/gunman47/37077267100/in/photolist-Xst9mG-JMYrh-YuoFKS-2iv6iDA-48jdNv-oCZhM6-opU96Z-7FwzH5-7FwzTm-o9UV7N-NtHhkd-9RQyvV)
-   [7. Central Water Catchment photography by CW
    Gan](https://www.flickr.com/photos/gancw1/41754717620/in/photolist-26BHQQu-27ZnxXM-2jMae6A-4oR778-cp4c1Q-2jMae5Z-2jM5NBQ-28gVPJ5-28h5Gcd-27Znyig-29nvBt4-29iecwo-26BHRuf-26BHQSy-4oXyNQ-4p8q41-4oTvq8-4oXyUs-Sysczd-4BJDiH-4oTze6-G5A7h-SB9Xe4-26BHRaN-26BHR2S-272S1B-26BHQWG-26BHReq-bkU6Zy-27Zny8g-27Zny5F-4oKwZB-4oKx6c-byNYg8-fdYdKs-4p4ms2-272RV6-26BHRZJ-277ijj-4pefyr-4oTvwK-4p4miP-4DFfFe-4p4mo8-4DKusW-26BHQYf-4oKx3H-4p4mhk-4BNKdN-FVEP5)
-   [8. Northshore photography by Steel
    Wool](https://www.flickr.com/photos/wynnie/7232205516/in/photolist-c25WJy-28d8aTw-c25TFU-fDeFiT-c25Wq3-NQniYf-Vt1gzM-wL6qfk-cBjohb-hyiq8i-cBjoyN-hygGJw-2i1Wv2x-c25Uoo-c25Tjs-fDeHEP-c25WWE-c25Vhw-c25Xx1-c25Vxf-c25Xfb-c25TWG-c25W5o-c25VN9-c25Uad-c25SH3-c25UPf-c25SVQ-c25V19-c25UAN-fDw9Fm-fDeEhB-fDf1dt-fDeJ8F-hyioiM-cBjnHo-hygTXo-2kJUSS-fDeDHa-fDwgXo-fDeWcc-fDeCx2-fDwhro-fDwaSd-fDwg3L-fDeJwt-hygUKf-hyitqR-fDwwus-hyhnyd)
-   [9. Serangoon Centre photography by Ellen
    Forsyth](https://www.flickr.com/photos/ellf/8409937430/in/photolist-dPa8pd-9MFzf7-dP4rHD-7bFYeH-7bFYeF-dP4tMa-78GKXb-dP4tzH-dP4pFp-dP4qpc-dP4sWF-dP4tjk-dPa6d3-dPa3wh-dPa5m3-dPa2wS-dP4u3P-dP4rQ4-dPa7M3-dPa6p9-dP9YzG-dPa4z9-dPa8as-dP9Zwm-dPa6PY-dP4syM-dP4vy4-dPa4NQ-8rSNck-dP4v9c-dPa3iY-2hJHQhg-dP4rgR-dPa3DS-dP4uAa-dPa4Yd-dP4ntR-eNSisJ-dP4rvr-dP4qAH-2iCnxqK-5S2zFs-8Ragv2-8419A8-5Yma9Q-8zKDPW-bJQ45B-b3ErW-4sVcWh-impgN5)
-   [10. Jurong East photography by Edsel
    Little](https://www.flickr.com/photos/edsel_/28009328562/in/photolist-JF68xY-HTJSDF-24TRubm-2coooYE-JQ85Mx-HTKfE4-JHdugK-HTKay2-uVauU-HTJL9r-JQ5gJD-cSjsx-JEZTnd-ef5yUz-ejtNDs-8cHrMz-7Zrhr6-7QXRYr-22uSBnK-22s6MX5-8azi5a-JM53WU-22s42Ys-JpjoRb-22uT9bD-4C7sx9-21gDFSG-JF66Fw-JHd2HK-JQ8za4-HTMgZW-JQ8wPx-8cH3t1-JQ3nxM-8CdUwk-JM4PWf-JHemmp-JQ3Vec-JM4Mfd-JQ8Eec-HTM3XU-24eAawG-8cHrMK-JQ8cua-4C3aoR-22uXQUe-6TsdYW-HTKRGt-8cHrMP-5tgHAH)
