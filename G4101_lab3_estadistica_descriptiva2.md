---
title: "Lab 3: Estadistica Descriptiva 2"
author: "Maximiliano Garnier Villarreal"
output:
  html_document:
    df_print: paged
    highlight: textmate
    keep_md: yes
    number_sections: yes
    theme: spacelab
    toc: yes
  word_document:
    toc: yes
  pdf_document:
    df_print: kable
    keep_tex: yes
    number_sections: yes
    toc: yes
  html_notebook:
    highlight: pygments
    number_sections: yes
    theme: flatly
    toc: yes
always_allow_html: yes
---

# Estadistica descriptiva




Vamos a usar los datos de `airquality`.


```r
airq = airquality %>% 
  as_tibble()
airq
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Ozone"],"name":[1],"type":["int"],"align":["right"]},{"label":["Solar.R"],"name":[2],"type":["int"],"align":["right"]},{"label":["Wind"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Temp"],"name":[4],"type":["int"],"align":["right"]},{"label":["Month"],"name":[5],"type":["int"],"align":["right"]},{"label":["Day"],"name":[6],"type":["int"],"align":["right"]}],"data":[{"1":"41","2":"190","3":"7.4","4":"67","5":"5","6":"1"},{"1":"36","2":"118","3":"8.0","4":"72","5":"5","6":"2"},{"1":"12","2":"149","3":"12.6","4":"74","5":"5","6":"3"},{"1":"18","2":"313","3":"11.5","4":"62","5":"5","6":"4"},{"1":"NA","2":"NA","3":"14.3","4":"56","5":"5","6":"5"},{"1":"28","2":"NA","3":"14.9","4":"66","5":"5","6":"6"},{"1":"23","2":"299","3":"8.6","4":"65","5":"5","6":"7"},{"1":"19","2":"99","3":"13.8","4":"59","5":"5","6":"8"},{"1":"8","2":"19","3":"20.1","4":"61","5":"5","6":"9"},{"1":"NA","2":"194","3":"8.6","4":"69","5":"5","6":"10"},{"1":"7","2":"NA","3":"6.9","4":"74","5":"5","6":"11"},{"1":"16","2":"256","3":"9.7","4":"69","5":"5","6":"12"},{"1":"11","2":"290","3":"9.2","4":"66","5":"5","6":"13"},{"1":"14","2":"274","3":"10.9","4":"68","5":"5","6":"14"},{"1":"18","2":"65","3":"13.2","4":"58","5":"5","6":"15"},{"1":"14","2":"334","3":"11.5","4":"64","5":"5","6":"16"},{"1":"34","2":"307","3":"12.0","4":"66","5":"5","6":"17"},{"1":"6","2":"78","3":"18.4","4":"57","5":"5","6":"18"},{"1":"30","2":"322","3":"11.5","4":"68","5":"5","6":"19"},{"1":"11","2":"44","3":"9.7","4":"62","5":"5","6":"20"},{"1":"1","2":"8","3":"9.7","4":"59","5":"5","6":"21"},{"1":"11","2":"320","3":"16.6","4":"73","5":"5","6":"22"},{"1":"4","2":"25","3":"9.7","4":"61","5":"5","6":"23"},{"1":"32","2":"92","3":"12.0","4":"61","5":"5","6":"24"},{"1":"NA","2":"66","3":"16.6","4":"57","5":"5","6":"25"},{"1":"NA","2":"266","3":"14.9","4":"58","5":"5","6":"26"},{"1":"NA","2":"NA","3":"8.0","4":"57","5":"5","6":"27"},{"1":"23","2":"13","3":"12.0","4":"67","5":"5","6":"28"},{"1":"45","2":"252","3":"14.9","4":"81","5":"5","6":"29"},{"1":"115","2":"223","3":"5.7","4":"79","5":"5","6":"30"},{"1":"37","2":"279","3":"7.4","4":"76","5":"5","6":"31"},{"1":"NA","2":"286","3":"8.6","4":"78","5":"6","6":"1"},{"1":"NA","2":"287","3":"9.7","4":"74","5":"6","6":"2"},{"1":"NA","2":"242","3":"16.1","4":"67","5":"6","6":"3"},{"1":"NA","2":"186","3":"9.2","4":"84","5":"6","6":"4"},{"1":"NA","2":"220","3":"8.6","4":"85","5":"6","6":"5"},{"1":"NA","2":"264","3":"14.3","4":"79","5":"6","6":"6"},{"1":"29","2":"127","3":"9.7","4":"82","5":"6","6":"7"},{"1":"NA","2":"273","3":"6.9","4":"87","5":"6","6":"8"},{"1":"71","2":"291","3":"13.8","4":"90","5":"6","6":"9"},{"1":"39","2":"323","3":"11.5","4":"87","5":"6","6":"10"},{"1":"NA","2":"259","3":"10.9","4":"93","5":"6","6":"11"},{"1":"NA","2":"250","3":"9.2","4":"92","5":"6","6":"12"},{"1":"23","2":"148","3":"8.0","4":"82","5":"6","6":"13"},{"1":"NA","2":"332","3":"13.8","4":"80","5":"6","6":"14"},{"1":"NA","2":"322","3":"11.5","4":"79","5":"6","6":"15"},{"1":"21","2":"191","3":"14.9","4":"77","5":"6","6":"16"},{"1":"37","2":"284","3":"20.7","4":"72","5":"6","6":"17"},{"1":"20","2":"37","3":"9.2","4":"65","5":"6","6":"18"},{"1":"12","2":"120","3":"11.5","4":"73","5":"6","6":"19"},{"1":"13","2":"137","3":"10.3","4":"76","5":"6","6":"20"},{"1":"NA","2":"150","3":"6.3","4":"77","5":"6","6":"21"},{"1":"NA","2":"59","3":"1.7","4":"76","5":"6","6":"22"},{"1":"NA","2":"91","3":"4.6","4":"76","5":"6","6":"23"},{"1":"NA","2":"250","3":"6.3","4":"76","5":"6","6":"24"},{"1":"NA","2":"135","3":"8.0","4":"75","5":"6","6":"25"},{"1":"NA","2":"127","3":"8.0","4":"78","5":"6","6":"26"},{"1":"NA","2":"47","3":"10.3","4":"73","5":"6","6":"27"},{"1":"NA","2":"98","3":"11.5","4":"80","5":"6","6":"28"},{"1":"NA","2":"31","3":"14.9","4":"77","5":"6","6":"29"},{"1":"NA","2":"138","3":"8.0","4":"83","5":"6","6":"30"},{"1":"135","2":"269","3":"4.1","4":"84","5":"7","6":"1"},{"1":"49","2":"248","3":"9.2","4":"85","5":"7","6":"2"},{"1":"32","2":"236","3":"9.2","4":"81","5":"7","6":"3"},{"1":"NA","2":"101","3":"10.9","4":"84","5":"7","6":"4"},{"1":"64","2":"175","3":"4.6","4":"83","5":"7","6":"5"},{"1":"40","2":"314","3":"10.9","4":"83","5":"7","6":"6"},{"1":"77","2":"276","3":"5.1","4":"88","5":"7","6":"7"},{"1":"97","2":"267","3":"6.3","4":"92","5":"7","6":"8"},{"1":"97","2":"272","3":"5.7","4":"92","5":"7","6":"9"},{"1":"85","2":"175","3":"7.4","4":"89","5":"7","6":"10"},{"1":"NA","2":"139","3":"8.6","4":"82","5":"7","6":"11"},{"1":"10","2":"264","3":"14.3","4":"73","5":"7","6":"12"},{"1":"27","2":"175","3":"14.9","4":"81","5":"7","6":"13"},{"1":"NA","2":"291","3":"14.9","4":"91","5":"7","6":"14"},{"1":"7","2":"48","3":"14.3","4":"80","5":"7","6":"15"},{"1":"48","2":"260","3":"6.9","4":"81","5":"7","6":"16"},{"1":"35","2":"274","3":"10.3","4":"82","5":"7","6":"17"},{"1":"61","2":"285","3":"6.3","4":"84","5":"7","6":"18"},{"1":"79","2":"187","3":"5.1","4":"87","5":"7","6":"19"},{"1":"63","2":"220","3":"11.5","4":"85","5":"7","6":"20"},{"1":"16","2":"7","3":"6.9","4":"74","5":"7","6":"21"},{"1":"NA","2":"258","3":"9.7","4":"81","5":"7","6":"22"},{"1":"NA","2":"295","3":"11.5","4":"82","5":"7","6":"23"},{"1":"80","2":"294","3":"8.6","4":"86","5":"7","6":"24"},{"1":"108","2":"223","3":"8.0","4":"85","5":"7","6":"25"},{"1":"20","2":"81","3":"8.6","4":"82","5":"7","6":"26"},{"1":"52","2":"82","3":"12.0","4":"86","5":"7","6":"27"},{"1":"82","2":"213","3":"7.4","4":"88","5":"7","6":"28"},{"1":"50","2":"275","3":"7.4","4":"86","5":"7","6":"29"},{"1":"64","2":"253","3":"7.4","4":"83","5":"7","6":"30"},{"1":"59","2":"254","3":"9.2","4":"81","5":"7","6":"31"},{"1":"39","2":"83","3":"6.9","4":"81","5":"8","6":"1"},{"1":"9","2":"24","3":"13.8","4":"81","5":"8","6":"2"},{"1":"16","2":"77","3":"7.4","4":"82","5":"8","6":"3"},{"1":"78","2":"NA","3":"6.9","4":"86","5":"8","6":"4"},{"1":"35","2":"NA","3":"7.4","4":"85","5":"8","6":"5"},{"1":"66","2":"NA","3":"4.6","4":"87","5":"8","6":"6"},{"1":"122","2":"255","3":"4.0","4":"89","5":"8","6":"7"},{"1":"89","2":"229","3":"10.3","4":"90","5":"8","6":"8"},{"1":"110","2":"207","3":"8.0","4":"90","5":"8","6":"9"},{"1":"NA","2":"222","3":"8.6","4":"92","5":"8","6":"10"},{"1":"NA","2":"137","3":"11.5","4":"86","5":"8","6":"11"},{"1":"44","2":"192","3":"11.5","4":"86","5":"8","6":"12"},{"1":"28","2":"273","3":"11.5","4":"82","5":"8","6":"13"},{"1":"65","2":"157","3":"9.7","4":"80","5":"8","6":"14"},{"1":"NA","2":"64","3":"11.5","4":"79","5":"8","6":"15"},{"1":"22","2":"71","3":"10.3","4":"77","5":"8","6":"16"},{"1":"59","2":"51","3":"6.3","4":"79","5":"8","6":"17"},{"1":"23","2":"115","3":"7.4","4":"76","5":"8","6":"18"},{"1":"31","2":"244","3":"10.9","4":"78","5":"8","6":"19"},{"1":"44","2":"190","3":"10.3","4":"78","5":"8","6":"20"},{"1":"21","2":"259","3":"15.5","4":"77","5":"8","6":"21"},{"1":"9","2":"36","3":"14.3","4":"72","5":"8","6":"22"},{"1":"NA","2":"255","3":"12.6","4":"75","5":"8","6":"23"},{"1":"45","2":"212","3":"9.7","4":"79","5":"8","6":"24"},{"1":"168","2":"238","3":"3.4","4":"81","5":"8","6":"25"},{"1":"73","2":"215","3":"8.0","4":"86","5":"8","6":"26"},{"1":"NA","2":"153","3":"5.7","4":"88","5":"8","6":"27"},{"1":"76","2":"203","3":"9.7","4":"97","5":"8","6":"28"},{"1":"118","2":"225","3":"2.3","4":"94","5":"8","6":"29"},{"1":"84","2":"237","3":"6.3","4":"96","5":"8","6":"30"},{"1":"85","2":"188","3":"6.3","4":"94","5":"8","6":"31"},{"1":"96","2":"167","3":"6.9","4":"91","5":"9","6":"1"},{"1":"78","2":"197","3":"5.1","4":"92","5":"9","6":"2"},{"1":"73","2":"183","3":"2.8","4":"93","5":"9","6":"3"},{"1":"91","2":"189","3":"4.6","4":"93","5":"9","6":"4"},{"1":"47","2":"95","3":"7.4","4":"87","5":"9","6":"5"},{"1":"32","2":"92","3":"15.5","4":"84","5":"9","6":"6"},{"1":"20","2":"252","3":"10.9","4":"80","5":"9","6":"7"},{"1":"23","2":"220","3":"10.3","4":"78","5":"9","6":"8"},{"1":"21","2":"230","3":"10.9","4":"75","5":"9","6":"9"},{"1":"24","2":"259","3":"9.7","4":"73","5":"9","6":"10"},{"1":"44","2":"236","3":"14.9","4":"81","5":"9","6":"11"},{"1":"21","2":"259","3":"15.5","4":"76","5":"9","6":"12"},{"1":"28","2":"238","3":"6.3","4":"77","5":"9","6":"13"},{"1":"9","2":"24","3":"10.9","4":"71","5":"9","6":"14"},{"1":"13","2":"112","3":"11.5","4":"71","5":"9","6":"15"},{"1":"46","2":"237","3":"6.9","4":"78","5":"9","6":"16"},{"1":"18","2":"224","3":"13.8","4":"67","5":"9","6":"17"},{"1":"13","2":"27","3":"10.3","4":"76","5":"9","6":"18"},{"1":"24","2":"238","3":"10.3","4":"68","5":"9","6":"19"},{"1":"16","2":"201","3":"8.0","4":"82","5":"9","6":"20"},{"1":"13","2":"238","3":"12.6","4":"64","5":"9","6":"21"},{"1":"23","2":"14","3":"9.2","4":"71","5":"9","6":"22"},{"1":"36","2":"139","3":"10.3","4":"81","5":"9","6":"23"},{"1":"7","2":"49","3":"10.3","4":"69","5":"9","6":"24"},{"1":"14","2":"20","3":"16.6","4":"63","5":"9","6":"25"},{"1":"30","2":"193","3":"6.9","4":"70","5":"9","6":"26"},{"1":"NA","2":"145","3":"13.2","4":"77","5":"9","6":"27"},{"1":"14","2":"191","3":"14.3","4":"75","5":"9","6":"28"},{"1":"18","2":"131","3":"8.0","4":"76","5":"9","6":"29"},{"1":"20","2":"223","3":"11.5","4":"68","5":"9","6":"30"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Bivariable

Las funciones covarianza (`cov`) y correlacion (`cor`) pueden actuar sobre vectores o sobre DataFrames. En estos ultimos el resultado va a ser una matriz de covarianza o correlacion, en su mayoria se requiere que el DataFrame sea numerico, sin columnas de texto o factores.


```r
x = airq$Solar.R
y = airq$Temp

cov(x,y,use = 'pairwise.complete.obs') # covarianza
```

```
## [1] 229.1598
```

```r
cor(x,y,method = "pearson",
    use = 'pairwise.complete.obs') # coeficiente de correlacion de Pearson
```

```
## [1] 0.2758403
```

```r
cov(airq, use = 'pairwise.complete.obs') # matriz de varianza-covarianza
```

```
##               Ozone     Solar.R        Wind       Temp      Month          Day
## Ozone   1088.200525 1056.583456 -70.9385307 218.521214  8.0089205   -3.8175412
## Solar.R 1056.583456 8110.519414 -17.9459707 229.159754 -9.5222485 -119.0259802
## Wind     -70.938531  -17.945971  12.4115385 -15.272136 -0.8897532    0.8488519
## Temp     218.521214  229.159754 -15.2721362  89.591331  5.6439628  -10.9574303
## Month      8.008921   -9.522248  -0.8897532   5.643963  2.0065359   -0.0999742
## Day       -3.817541 -119.025980   0.8488519 -10.957430 -0.0999742   78.5797214
```

```r
cor(airq, use = 'pairwise.complete.obs') # matriz de correlacion de Pearson
```

```
##               Ozone     Solar.R        Wind       Temp        Month
## Ozone    1.00000000  0.34834169 -0.60154653  0.6983603  0.164519314
## Solar.R  0.34834169  1.00000000 -0.05679167  0.2758403 -0.075300764
## Wind    -0.60154653 -0.05679167  1.00000000 -0.4579879 -0.178292579
## Temp     0.69836034  0.27584027 -0.45798788  1.0000000  0.420947252
## Month    0.16451931 -0.07530076 -0.17829258  0.4209473  1.000000000
## Day     -0.01322565 -0.15027498  0.02718090 -0.1305932 -0.007961763
##                  Day
## Ozone   -0.013225647
## Solar.R -0.150274979
## Wind     0.027180903
## Temp    -0.130593175
## Month   -0.007961763
## Day      1.000000000
```

```r
correlation(airq, method = 'pearson')
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter1"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Parameter2"],"name":[2],"type":["chr"],"align":["left"]},{"label":["r"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["CI"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CI_low"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["CI_high"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["t"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["df_error"],"name":[8],"type":["int"],"align":["right"]},{"label":["p"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Method"],"name":[10],"type":["chr"],"align":["left"]},{"label":["n_Obs"],"name":[11],"type":["int"],"align":["right"]}],"data":[{"1":"Ozone","2":"Solar.R","3":"0.348341693","4":"0.95","5":"0.17319400","6":"0.50213196","7":"3.87979481","8":"109","9":"1.972419e-03","10":"Pearson correlation","11":"111","_rn_":"1"},{"1":"Ozone","2":"Wind","3":"-0.601546530","4":"0.95","5":"-0.70639179","6":"-0.47087128","7":"-8.04012988","8":"114","9":"1.298076e-11","10":"Pearson correlation","11":"116","_rn_":"2"},{"1":"Ozone","2":"Temp","3":"0.698360342","4":"0.95","5":"0.59133397","6":"0.78121106","7":"10.41772418","8":"114","9":"4.397845e-17","10":"Pearson correlation","11":"116","_rn_":"3"},{"1":"Ozone","2":"Month","3":"0.164519314","4":"0.95","5":"-0.01834762","6":"0.33673567","7":"1.78085173","8":"114","9":"5.617870e-01","10":"Pearson correlation","11":"116","_rn_":"4"},{"1":"Ozone","2":"Day","3":"-0.013225647","4":"0.95","5":"-0.19507188","6":"0.16949967","7":"-0.14122361","8":"114","9":"1.000000e+00","10":"Pearson correlation","11":"116","_rn_":"5"},{"1":"Solar.R","2":"Wind","3":"-0.056791666","4":"0.95","5":"-0.21723589","6":"0.10664055","7":"-0.68260167","8":"144","9":"1.000000e+00","10":"Pearson correlation","11":"146","_rn_":"6"},{"1":"Solar.R","2":"Temp","3":"0.275840271","4":"0.95","5":"0.11871132","6":"0.41949133","7":"3.44368627","8":"144","9":"7.517729e-03","10":"Pearson correlation","11":"146","_rn_":"7"},{"1":"Solar.R","2":"Month","3":"-0.075300764","4":"0.95","5":"-0.23487603","6":"0.08822685","7":"-0.90618194","8":"144","9":"1.000000e+00","10":"Pearson correlation","11":"146","_rn_":"8"},{"1":"Solar.R","2":"Day","3":"-0.150274979","4":"0.95","5":"-0.30527111","6":"0.01247802","7":"-1.82401281","8":"144","9":"5.617870e-01","10":"Pearson correlation","11":"146","_rn_":"9"},{"1":"Wind","2":"Temp","3":"-0.457987879","4":"0.95","5":"-0.57488741","6":"-0.32276602","7":"-6.33083510","8":"151","9":"3.434076e-08","10":"Pearson correlation","11":"153","_rn_":"10"},{"1":"Wind","2":"Month","3":"-0.178292579","4":"0.95","5":"-0.32769971","6":"-0.02018552","7":"-2.22657109","8":"151","9":"2.471060e-01","10":"Pearson correlation","11":"153","_rn_":"11"},{"1":"Wind","2":"Day","3":"0.027180903","4":"0.95","5":"-0.13206683","6":"0.18506086","7":"0.33412798","8":"151","9":"1.000000e+00","10":"Pearson correlation","11":"153","_rn_":"12"},{"1":"Temp","2":"Month","3":"0.420947252","4":"0.95","5":"0.28104131","6":"0.54333336","7":"5.70253699","8":"151","9":"7.231443e-07","10":"Pearson correlation","11":"153","_rn_":"13"},{"1":"Temp","2":"Day","3":"-0.130593175","4":"0.95","5":"-0.28339864","6":"0.02867925","7":"-1.61861758","8":"151","9":"6.456986e-01","10":"Pearson correlation","11":"153","_rn_":"14"},{"1":"Month","2":"Day","3":"-0.007961763","4":"0.95","5":"-0.16642963","6":"0.15090702","7":"-0.09783888","8":"151","9":"1.000000e+00","10":"Pearson correlation","11":"153","_rn_":"15"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# grafico estatico de coeficientes de correlacion

GGally::ggcorr(airq, label = T,
               label_round = 2,
               label_size = 3,
               method = c('pairwise.complete.obs','pearson')) 
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggstatsplot::ggcorrmat(airq)
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

El siguiente codigo genera un grafico interactivo de la matriz de correlacion (Solo se despliega en formato HTML).


```r
hchart(cor(airq,use = 'pairwise.complete.obs')) # grafico interactivo de coeficientes de correlacion
```

```{=html}
<div id="htmlwidget-2c14e601d11b267df596" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-2c14e601d11b267df596">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":null},"yAxis":{"title":{"text":""},"categories":["Ozone","Solar.R","Wind","Temp","Month","Day"],"reversed":true},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false,"boderWidth":0,"dataLabels":{"enabled":false}},"treemap":{"layoutAlgorithm":"squarified"}},"series":[{"data":[{"x":0,"y":0,"value":1,"name":"Ozone ~ Ozone"},{"x":0,"y":1,"value":0.348341692993603,"name":"Ozone ~ Solar.R"},{"x":0,"y":2,"value":-0.60154652988895,"name":"Ozone ~ Wind"},{"x":0,"y":3,"value":0.698360342150932,"name":"Ozone ~ Temp"},{"x":0,"y":4,"value":0.164519314380413,"name":"Ozone ~ Month"},{"x":0,"y":5,"value":-0.013225646554047,"name":"Ozone ~ Day"},{"x":1,"y":0,"value":0.348341692993603,"name":"Solar.R ~ Ozone"},{"x":1,"y":1,"value":1,"name":"Solar.R ~ Solar.R"},{"x":1,"y":2,"value":-0.0567916657698467,"name":"Solar.R ~ Wind"},{"x":1,"y":3,"value":0.275840271340805,"name":"Solar.R ~ Temp"},{"x":1,"y":4,"value":-0.0753007638859408,"name":"Solar.R ~ Month"},{"x":1,"y":5,"value":-0.150274979240985,"name":"Solar.R ~ Day"},{"x":2,"y":0,"value":-0.60154652988895,"name":"Wind ~ Ozone"},{"x":2,"y":1,"value":-0.0567916657698467,"name":"Wind ~ Solar.R"},{"x":2,"y":2,"value":1,"name":"Wind ~ Wind"},{"x":2,"y":3,"value":-0.457987879104833,"name":"Wind ~ Temp"},{"x":2,"y":4,"value":-0.17829257921769,"name":"Wind ~ Month"},{"x":2,"y":5,"value":0.027180902809146,"name":"Wind ~ Day"},{"x":3,"y":0,"value":0.698360342150932,"name":"Temp ~ Ozone"},{"x":3,"y":1,"value":0.275840271340805,"name":"Temp ~ Solar.R"},{"x":3,"y":2,"value":-0.457987879104833,"name":"Temp ~ Wind"},{"x":3,"y":3,"value":1,"name":"Temp ~ Temp"},{"x":3,"y":4,"value":0.420947252266222,"name":"Temp ~ Month"},{"x":3,"y":5,"value":-0.130593175159278,"name":"Temp ~ Day"},{"x":4,"y":0,"value":0.164519314380413,"name":"Month ~ Ozone"},{"x":4,"y":1,"value":-0.0753007638859408,"name":"Month ~ Solar.R"},{"x":4,"y":2,"value":-0.17829257921769,"name":"Month ~ Wind"},{"x":4,"y":3,"value":0.420947252266222,"name":"Month ~ Temp"},{"x":4,"y":4,"value":1,"name":"Month ~ Month"},{"x":4,"y":5,"value":-0.00796176260045312,"name":"Month ~ Day"},{"x":5,"y":0,"value":-0.013225646554047,"name":"Day ~ Ozone"},{"x":5,"y":1,"value":-0.150274979240985,"name":"Day ~ Solar.R"},{"x":5,"y":2,"value":0.027180902809146,"name":"Day ~ Wind"},{"x":5,"y":3,"value":-0.130593175159278,"name":"Day ~ Temp"},{"x":5,"y":4,"value":-0.00796176260045312,"name":"Day ~ Month"},{"x":5,"y":5,"value":1,"name":"Day ~ Day"}],"type":"heatmap"}],"tooltip":{"formatter":"function(){\n                 return this.point.name + ': ' +\n                   Highcharts.numberFormat(this.point.value, 2)\n               }"},"legend":{"enabled":true},"colorAxis":{"auxarg":true,"stops":[[0,"#FF5733"],[0.5,"#F8F5F5"],[1,"#2E86C1"]],"min":-1,"max":1},"xAxis":{"categories":["Ozone","Solar.R","Wind","Temp","Month","Day"],"title":{"text":""},"opposite":true}},"theme":{"chart":{"backgroundColor":"transparent"},"colors":["#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#2b908f","#f45b5b","#91e8e1"]},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>
```

## Regresion

Para presentar caracteristicas de los modelos de forma mas estetica y ordenada se pueden usar las funciones `tidy` y `glance` del paquete *broom*, o `model_parameter` y `model_performance` de los paquetes *parameters* y *performance*. `tidy` y `model_parameter` genera la tabla resumen de los coeficientes y sus estadisticas, mientras que `glance` y `model_performance` genera una tabla con medidas de ajuste, incluyendo el $R^2$.

### Lineal

$y \thicksim \hat{b}_0 + x\hat{b}_1 + \epsilon$


```r
mod1 = lm(Temp ~ Solar.R, data = airq) # modelo de regresion lineal

mod1 %>% tidy()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"(Intercept)","2":"72.86301176","3":"1.693950864","4":"43.013651","5":"4.541566e-84"},{"1":"Solar.R","2":"0.02825463","3":"0.008204764","4":"3.443686","5":"7.517729e-04"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod1 %>% glance()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["r.squared"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["adj.r.squared"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["sigma"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["df.residual"],"name":[11],"type":["int"],"align":["right"]},{"label":["nobs"],"name":[12],"type":["int"],"align":["right"]}],"data":[{"1":"0.07608786","2":"0.0696718","3":"8.897632","4":"11.85898","5":"0.0007517729","6":"1","7":"-525.2827","8":"1056.565","9":"1065.516","10":"11400.17","11":"144","12":"146","_row":"value"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod1 %>% model_parameters()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Coefficient"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["SE"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["CI"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CI_low"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["CI_high"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["t"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["df_error"],"name":[8],"type":["int"],"align":["right"]},{"label":["p"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"(Intercept)","2":"72.86301176","3":"1.693950864","4":"0.95","5":"69.5147907","6":"76.21123280","7":"43.013651","8":"144","9":"4.541566e-84","_rn_":"1"},{"1":"Solar.R","2":"0.02825463","3":"0.008204764","4":"0.95","5":"0.0120373","6":"0.04447197","7":"3.443686","8":"144","9":"7.517729e-04","_rn_":"2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod1 %>% model_performance()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["AIC"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["R2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["R2_adjusted"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSE"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Sigma"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1056.565","2":"1065.516","3":"0.07608786","4":"0.0696718","5":"8.836479","6":"8.897632","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
sjstats::cv(mod1) # cv
```

```
## [1] 0.1131193
```

```r
# graficos diagnostico
# plot(mod1) # graficos diagnostico
check_model(mod1,panel=F)
```

```
## Loading required namespace: qqplotr
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-4-3.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-4-4.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-4-5.png)<!-- -->

### Cuadratica

$y \thicksim \hat{b}_0 + x\hat{b}_1 + x^2\hat{b}_2 + \epsilon$


```r
mod2 = lm(Temp ~ Solar.R + I(Solar.R^2), data = airq) # modelo de regresion cuadratica

mod2 %>% tidy()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["chr"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"(Intercept)","2":"64.2677293059","3":"2.397337e+00","4":"26.807969","5":"1.236156e-57"},{"1":"Solar.R","2":"0.1758133953","3":"3.189690e-02","4":"5.511928","5":"1.608610e-07"},{"1":"I(Solar.R^2)","2":"-0.0004420017","3":"9.275735e-05","4":"-4.765139","5":"4.588509e-06"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod2 %>% glance()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["r.squared"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["adj.r.squared"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["sigma"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["df"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["logLik"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["AIC"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["deviance"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["df.residual"],"name":[11],"type":["int"],"align":["right"]},{"label":["nobs"],"name":[12],"type":["int"],"align":["right"]}],"data":[{"1":"0.2026903","2":"0.1915391","3":"8.294417","4":"18.17657","5":"9.254073e-08","6":"2","7":"-514.5245","8":"1037.049","9":"1048.983","10":"9838.02","11":"143","12":"146","_row":"value"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod2 %>% model_parameters()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Parameter"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Coefficient"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["SE"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["CI"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["CI_low"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["CI_high"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["t"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["df_error"],"name":[8],"type":["int"],"align":["right"]},{"label":["p"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"(Intercept)","2":"64.2677293059","3":"2.397337e+00","4":"0.95","5":"59.5289323899","6":"69.006526222","7":"26.807969","8":"143","9":"1.236156e-57","_rn_":"1"},{"1":"Solar.R","2":"0.1758133953","3":"3.189690e-02","4":"0.95","5":"0.1127630478","6":"0.238863743","7":"5.511928","8":"143","9":"1.608610e-07","_rn_":"2"},{"1":"I(Solar.R^2)","2":"-0.0004420017","3":"9.275735e-05","4":"0.95","5":"-0.0006253544","6":"-0.000258649","7":"-4.765139","8":"143","9":"4.588509e-06","_rn_":"3"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
mod2 %>% model_performance()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["AIC"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["R2"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["R2_adjusted"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["RMSE"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["Sigma"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1037.049","2":"1048.983","3":"0.2026903","4":"0.1915391","5":"8.208758","6":"8.294417","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
sjstats::cv(mod2) # cv
```

```
## [1] 0.1050836
```

```r
# graficos diagnostico
# plot(mod2) 
check_model(mod2,panel=F)
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-2.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-3.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-4.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-5.png)<!-- -->![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

### Compara modelos

```r
TMod(mod1,mod2) # DescTools
```

```
##             coef      mod1         mod2    
## 1    (Intercept)    72.863 ***   64.268 ***
## 2        Solar.R     0.028 ***    0.176 ***
## 3   I(Solar.R^2)     -           -0.000 ***
## 4            ---                           
## 5      r.squared     0.076        0.203    
## 6  adj.r.squared     0.070        0.192    
## 7          sigma     8.898        8.294    
## 8         logLik  -525.283     -514.524    
## 9       deviance 11400.171     9838.020    
## 10           AIC  1056.565     1037.049    
## 11           BIC  1065.516     1048.983    
## 12         numdf     1            2        
## 13         dendf   144          143        
## 14             N   146          146        
## 15        n vars     1            2        
## 16        n coef     2            3        
## 17             F    11.859       18.177    
## 18             p     0.001        9.254e-08
## 19           MAE     7.060        6.730    
## 20          MAPE     0.094        0.090    
## 21           MSE    78.083       67.384    
## 22          RMSE     8.836        8.209
```

```r
modelsummary::modelsummary(list(Lineal=mod1,Cuadratico=mod2),
                           statistic = 'conf.int',
                           stars = T)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Lineal </th>
   <th style="text-align:center;"> Cuadratico </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:center;"> 72.863*** </td>
   <td style="text-align:center;"> 64.268*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [69.515, 76.211] </td>
   <td style="text-align:center;"> [59.529, 69.007] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Solar.R </td>
   <td style="text-align:center;"> 0.028*** </td>
   <td style="text-align:center;"> 0.176*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [0.012, 0.044] </td>
   <td style="text-align:center;"> [0.113, 0.239] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> I(Solar.R^2) </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 0.000*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> [-0.001, 0.000] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 146 </td>
   <td style="text-align:center;"> 146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:center;"> 0.076 </td>
   <td style="text-align:center;"> 0.203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.070 </td>
   <td style="text-align:center;"> 0.192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> 1056.6 </td>
   <td style="text-align:center;"> 1037.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> 1065.5 </td>
   <td style="text-align:center;"> 1049.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Log.Lik. </td>
   <td style="text-align:center;"> -525.283 </td>
   <td style="text-align:center;"> -514.524 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F </td>
   <td style="text-align:center;"> 11.859 </td>
   <td style="text-align:center;"> 18.177 </td>
  </tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border:0;" colspan="100%">
<sup></sup> * p &lt; 0.1, ** p &lt; 0.05, *** p &lt; 0.01</td>
</tr>
</tfoot>
</table>

```r
sjPlot::tab_model(mod1,mod2,
                  title = 'Comparasion de modelos',
                  collapse.ci = T,
                  ci.hyphen = ' , ',
                  show.stat = T,
                  dv.labels = c('Lineal','Cuadrático'),
                  digits = 3)
```

<table style="border-collapse:collapse; border:none;">
<caption style="font-weight: bold; text-align:left;">Comparasion de modelos</caption>
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">Lineal</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">Cuadrático</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Estimates</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Statistic</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">72.863<br>(69.515 , 76.211)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">43.014</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>&lt;0.001</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">64.268<br>(59.529 , 69.007)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">26.808</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Solar.R</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.028<br>(0.012 , 0.044)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">3.444</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.001</strong></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.176<br>(0.113 , 0.239)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">5.512</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">Solar.R^2</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;0.000<br>(-0.001 , -0.000)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">&#45;4.765</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7"><strong>&lt;0.001</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">146</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">146</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">R<sup>2</sup> / R<sup>2</sup> adjusted</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.076 / 0.070</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.203 / 0.192</td>
</tr>

</table>

```r
comp.mods = compare_performance(mod1,mod2,rank = T) # performance
comp.mods
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Model"],"name":[2],"type":["chr"],"align":["left"]},{"label":["AIC"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["BIC"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["R2"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["R2_adjusted"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["RMSE"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Sigma"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Performance_Score"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"mod2","2":"lm","3":"1037.049","4":"1048.983","5":"0.20269032","6":"0.1915391","7":"8.208758","8":"8.294417","9":"1"},{"1":"mod1","2":"lm","3":"1056.565","4":"1065.516","5":"0.07608786","6":"0.0696718","7":"8.836479","8":"8.897632","9":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
plot(comp.mods)
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

### Graficos

Para agregar lineas de tendencia en *ggplot2* se usa la funcion `geom_smooth`. Por defecto ajusta una curva loess, pero para cambiarlo se usa `method = 'lm'`, y para especificar una ecuacion diferente a la regresion simple se usa `formula`, donde `y` y `x` son genericos, **NO** hay que poner el nombre de las variables que se esta graficando.


```r
ggplot(airq, aes(Solar.R,Temp)) + 
  geom_point()
```

```
## Warning: Removed 7 rows containing missing values (geom_point).
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
ggplot(airq, aes(Solar.R,Temp)) + 
  geom_point() + 
  geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

```
## Warning: Removed 7 rows containing non-finite values (stat_smooth).

## Warning: Removed 7 rows containing missing values (geom_point).
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
# grafico regresion lineal
ggplot(airq, aes(Solar.R,Temp)) + 
  geom_point() + 
  geom_smooth(method = 'lm')
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 7 rows containing non-finite values (stat_smooth).

## Warning: Removed 7 rows containing missing values (geom_point).
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
# grafico regresion cuadratica
ggplot(airq, aes(Solar.R,Temp)) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = y~poly(x, 2))
```

```
## Warning: Removed 7 rows containing non-finite values (stat_smooth).

## Warning: Removed 7 rows containing missing values (geom_point).
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```r
# grafico regresion lineal y cuadratica
ggplot(airq, aes(Solar.R,Temp)) + 
  geom_point() + 
  geom_smooth(method = 'lm', aes(col='Lineal')) + 
  geom_smooth(method = 'lm', formula = y~poly(x, 2), aes(col='Cuadratica'))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

```
## Warning: Removed 7 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 7 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 7 rows containing missing values (geom_point).
```

![](G4101_lab3_estadistica_descriptiva2_files/figure-html/unnamed-chunk-7-5.png)<!-- -->


# Probabilidad


```r
A = matrix(data = c(15,24,23,14,11,13),nrow = 2)
colnames(A) = c('A','B','C')
row.names(A) = c('X','Y')
PercTable(A)
```

```
##                           
##              A     B     C
##                           
## X   freq    15    23    11
##     perc 15.0% 23.0% 11.0%
##                           
## Y   freq    24    14    13
##     perc 24.0% 14.0% 13.0%
## 
```

```r
PercTable(A, margins = c(1,2))
```

```
##                                       
##                 A      B      C    Sum
##                                       
## X     freq     15     23     11     49
##       perc  15.0%  23.0%  11.0%  49.0%
##                                       
## Y     freq     24     14     13     51
##       perc  24.0%  14.0%  13.0%  51.0%
##                                       
## Sum   freq     39     37     24    100
##       perc  39.0%  37.0%  24.0% 100.0%
## 
```

