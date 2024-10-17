# Presentacion-Pakitoleon


<img align="center" src="https://github-readme-stats.vercel.app/api?username=Pakitoleon&include_all_commits=true&count_private=true&show_icons=true&line_height=20&title_color=2B5BBD&icon_color=1124BB&text_color=A1A1A1&bg_color=0,000000,130F40" alt="my Github Stats"/>

<img align="center" src="https://github-readme-streak-stats.herokuapp.com/?user=Pakitoleon&theme=tokyonight" alt="mystreak"/>


<img align="center" src="https://github-readme-stats.vercel.app/api/top-langs?username=Pakitoleon&show_icons=true&locale=en&layout=compact&theme=chartreuse-dark" alt="ovi" />



<table>
  <thead align="center">
    <tr border: none;>
      <td><b>📘 Project</b></td>
      <td><b>⭐ Stars</b></td>
      <td><b>🤝 Forks</b></td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><a href="https://github.com/Pakitoleon/get-next-line-42-malaga"><b>GNL</b></a></td>
      <td><img alt="Stars" src="https://img.shields.io/github/stars/Pakitoleon/get_next_line-42-malaga/?style=flat-square&labelColor=343b41"/></td>
      <td><img alt="Forks" src="https://img.shields.io/github/forks/Pakitoleon/get_next_line-42-malaga/?style=flat-square&labelColor=343b41"/></td>
    </tr>
    <tr>
      <td><a href="https://github.com/Pakitoleon/libft-42-malaga"><b>LIBFT</b></a></td>
      <td><img alt="Stars" src="https://img.shields.io/github/stars/Pakitoleon/libft-42-malaga?style=flat-square&labelColor=343b41"/></td>
      <td><img alt="Forks" src="https://img.shields.io/github/forks/Pakitoleon/libft-42-malaga/?style=flat-square&labelColor=343b41"/></td>
    </tr>
      <tbody>
    <tr>
      <td><a href="https://github.com/Pakitoleon/differ"><b>Differ</b></a></td>
      <td><img alt="Stars" src="https://img.shields.io/github/stars/Pakitoleon/differ?style=flat-square&labelColor=343b41"/></td>
      <td><img alt="Forks" src="https://img.shields.io/github/forks/Pakitoleon/differ?style=flat-square&labelColor=343b41"/></td>
    </tr>
  </tbody>
</table>

<img alt="followers" title="Follow me on Github" src="https://img.shields.io/github/followers/Pakitoleon?color=236ad3&style=for-the-badge&logo=github&label=Follow"/>


## setting up a bootstrap run

defaults <- 
  within(defaults,
         {area = mfdb_bootstrap_group(100,defaults$area,seed=1337)})

source('06-ling/00-setup/setup-catchdistribution.R')
source('06-ling/00-setup/setup-indices.R')
save.image(file='06-ling/00-setup/bootdata.R')

boot_setup <- function(i){
  var_dir <- gadget.variant.dir(gd$dir, variant_dir = paste0('BS.WGTS/BS.', i))

  aldist.igfs[[i]]$step <- 2
  ldist.igfs[[i]]$step <- 2
  matp.igfs[[i]]$step <- 2
  
  gadgetlikelihood('likelihood',var_dir,missingOkay = TRUE) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.igfs",
                  weight = 1,
                  data = ldist.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.igfs",
                  weight = 1,
                  data = aldist.igfs[[i]] %>% ## only two age samples in 1989
                    #filter(year!=1989),
                    filter(year>1998),
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.lln",
                  weight = 1,
                  data = ldist.lln[[i]] %>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
                    filter(year!=1993&step!=4),
                  fleetnames = c("lln"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.lln",
                  weight = 1,
                  data = aldist.lln[[i]] %>%  ## only 20 fish aged taken in those quarters
                    filter(year>1998,!((year==2002|year==2003)&step==2)),
                  fleetnames = c("lln"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.gil",
                  weight = 1,
                  data = ldist.gil[[i]] %>% ## only one fish lengthmeasured
                    filter(!(year==2005&step==2)),
                  fleetnames = c("gil"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.gil",
                  weight = 1,
                  data = aldist.gil[[i]] %>% 
                    filter(year>1998),
                  fleetnames = c("gil"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.bmt",
                  weight = 1,
                  data = ldist.bmt[[i]] %>% ## to few samples (<=20 fish lengths)
                    filter(!(year==1982&step==4),
                           !(year==1984&step==1),
                           !(year==1992&step==4),
                           !(year==1994&step==1),
                           !(year==1998&step==3),
                           !(year==1989&step==3)),
                  fleetnames = c("bmt"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.bmt",
                  weight = 1,
                  data = aldist.bmt[[i]] %>% 
                    filter(year>1998),
                  fleetnames = c("bmt"),
                  stocknames = stock_names) %>% 
    gadget_update("stockdistribution",
                  name = "matp.igfs",
                  weight = 1,
                  data = matp.igfs[[i]] %>% ## maturity @ length in 1985 appears to be silly and only one sample in 1989
                    filter(year>1989),
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
      gadget_update("surveyindices",
                  name = "si.20-50",
                  weight = 1,
                  data = igfs.SI1[[i]],
                  fittype = 'loglinearfit',
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.50-60",
                  weight = 1,
                  data = igfs.SI2a[[i]],
                  fittype = 'loglinearfit',
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.60-70",
                  weight = 1,
                  data = igfs.SI2b[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.70-80",
                  weight = 1,
                  data = igfs.SI3a[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.80-90",
                  weight = 1,
                  data = igfs.SI3b[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.90-100",
                  weight = 1,
                  data = igfs.SI3c[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.100-160",
                  weight = 1,
                  data = igfs.SI3d[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) -> tmp
  attr(tmp,'file_config')$mainfile_overwrite = TRUE 
    write.gadget.file(tmp, var_dir)
}

tmp <- 
  mclapply(seq_along(defaults$area),
           boot_setup,
           mc.cores = detectCores(logical = TRUE))
