library(tidyverse)
library(dataRetrieval)
library(ggpmisc)
#> Loading required package: ggpp
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2
#>
#> Attaching package: 'ggpp'
#> The following object is masked from 'package:ggplot2':
#>
#>     annotate

# 1. Download time series of river water flow
dat <- readNWISuv(siteNumbers = "01576381",
                  parameterCd = "00060",
                  startDate = "2024-04-01",
                  endDate = "2024-04-15",
                  tz = "America/New_York") %>%
  renameNWISColumns() %>%
  rename(flow = Flow_Inst) %>%
  select(dateTime, flow)
#> GET:https://nwis.waterservices.usgs.gov/nwis/iv/?site=01576381&format=waterml%2C1.1&ParameterCd=00060&startDT=2024-04-01&endDT=2024-04-15

# 2. See data structure
head(dat)
#>              dateTime flow
#> 1 2024-04-01 00:00:00 25.0
#> 2 2024-04-01 00:15:00 25.6
#> 3 2024-04-01 00:30:00 25.0
#> 4 2024-04-01 00:45:00 25.6
#> 5 2024-04-01 01:00:00 25.1
#> 6 2024-04-01 01:15:00 25.6

nrow(dat)
#> [1] 1440

# 3. Visualize time series of river flow over 2-week period
dat %>%
  ggplot(aes(x = dateTime, y = flow)) +
  geom_line() +
  stat_peaks(color = 'red') +
  theme_bw()

dat %>%
  ggplot(aes(x = dateTime, y = flow)) +
  geom_line() +
  stat_peaks(color = 'red', span = 25) +
  theme_bw()

dat %>%
  ggplot(aes(x = dateTime, y = flow)) +
  geom_line() +
  geom_point(shape = "circle small") +
  stat_peaks(color = 'red', span = 31,
             local.reference = "farthest",
             local.threshold = 0.01) +
  theme_bw()

dat %>%
  ggplot(aes(x = dateTime, y = flow)) +
  geom_line() +
  stat_peaks(color = 'red', span = 25,
             local.threshold = 0.015) +
  theme_bw()

dat %>%
  ggplot(aes(x = dateTime, y = flow)) +
  geom_line() +
  stat_peaks(color = 'red', span = 25,
             local.threshold = I(0.015 * 310)) +
  theme_bw()
