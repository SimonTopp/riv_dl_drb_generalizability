library(tidyverse)
library(lubridate)
library(reticulate)
library(sf)
library(plotly)
library(feather)
library(ggridges)
library(ggpubr)
library(knitr)
library(kableExtra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../../river-dl')
source('../drb_gwnet/2_Analysis/utils.R')

################
##Bring in some spatial data
################

spatial <- readRDS('data_DRB/DRB_spatial/network.rds')
edges <- spatial$edges %>% st_as_sf()
llo_groups <- read_csv('data_DRB/DRB_spatial/llo_groups.csv') %>%
  mutate(test_group=ifelse(test_group=='Piedmont','Headwaters',test_group))

reach_noise <- aggregate_xai('results/xai_outputs/noise_annual', 'reach_noise')

dams <- readRDS('data_DRB/DRB_spatial/filtered_dams_reservoirs.rds')[[1]] %>%
  filter(!GRAND_ID %in% c(1591, 1584, 2242, 1584, 2212)) #Not on a reach

plot_reach_noise <- function(df, mod, scenario, title, ll=F, ul=F){
    
  df <- df %>% filter(model == mod, run == scenario) %>%
      inner_join(edges)
  max = max(df$diffs)
  
  p1 <- ggplot(df,aes(x = diffs)) +
    geom_histogram() +
    xlim(0,8) +
    ggtitle(title)+
    theme_bw()
  
  
  p2 <- ggplot(df, aes(geometry = geometry)) + 
    geom_sf(aes(color = diffs)) + 
    scale_color_viridis_c(limits=c(0,max), direction = -1) + 
    ggthemes::theme_map() +
    theme(legend.position = 'right')
  if(ll){
    df <- df %>%
      mutate(diffs = ifelse(diffs < ll, ll, diffs),
             diffs = ifelse(diffs > ul, ul, diffs))
    p2 <- ggplot(df, aes(geometry = geometry)) + 
      geom_sf(aes(color = diffs)) + 
      geom_sf(data = dams, aes(fill = 'Dams'), size = .3) +  
      scale_color_viridis_c(limits = c(ll,ul),direction = -1) + 
      ggthemes::theme_map() +
      theme(legend.position = 'right') 
  }
  
  #gridExtra::grid.arrange(p1,p2, ncol=1,heights = c(.2,.8))
  ggpubr::ggarrange(p1,p2,ncol=1, heights = c(.2,.6))
}

p1<- plot_reach_noise(reach_noise,'GWN','ptft','GWN PT/FT Weights',ll=2,ul=5)
p2<- plot_reach_noise(reach_noise,'GWN','coast_ptft','GWN Head PT/FT Weights',ll=2,ul=5)

p1<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights',ll=2,ul=5)
p2<- plot_reach_noise(reach_noise,'RGCN','coast_ptft','RGCN Coast PT/FT Weights',ll=2,ul=5)

p2<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights',ll=2,ul=5)
gridExtra::grid.arrange(p1,p2,nrow=1)

p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','nopt','GWN No PT Weights')
p4 <- plot_reach_noise(reach_noise,'GWN','rs_adj','GWN RS Adj Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3,p4, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','nopt','RGCN No PT Weights')
p4 <- plot_reach_noise(reach_noise,'RGCN','rs_adj','GWN RS Adj Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3,p4, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)


p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','coast_ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','coast_nopt','GWN No PT Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','coast_ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','coast_nopt','RGCN No PT Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)



p1<- plot_reach_noise(reach_noise,'GWN','pt','GWN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'GWN','head_ptft','GWN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'GWN','head_nopt','GWN No PT Weights')


pgwn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)


p1<- plot_reach_noise(reach_noise,'RGCN','pt','RGCN Pretrain Weights')
p2<- plot_reach_noise(reach_noise,'RGCN','head_ptft','RGCN PT/FT Weights')
p3 <- plot_reach_noise(reach_noise,'RGCN','head_nopt','RGCN No PT Weights')

prgcn <- ggpubr::ggarrange(p1,p2,p3, nrow=1,common.legend = T)

ggpubr::ggarrange(pgwn,prgcn,nrow=2)


############## Baseline comps figs
baseline <- reach_noise %>% filter(run == 'nopt') %>%
  inner_join(edges)

p1 <- ggplot(baseline, aes(x = diffs)) +
  geom_histogram(aes(fill=model),position='identity', alpha = .6) +
  scale_fill_viridis_d(end = .7)+
  labs(x = 'Sensitivity to Spatial Noise (∆ ºC)',
       y = 'Reach \nCount',
       fill = 'Model') +
  theme_minimal()

ul = 7
ll = 4
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
p2 <- baseline %>%
  group_by(model) %>%
  #mutate(diffs = normalize(diffs)) %>%
  mutate(diffs = ifelse(diffs < ll, ll, diffs),
         diffs = ifelse(diffs > ul, ul, diffs)) %>%
  ggplot(., aes(geometry = geometry)) + 
    geom_sf(aes(color = diffs)) + 
    geom_sf(data = dams, aes(fill = 'Reservoirs'), size = .3) +  
    scale_color_viridis_c(limits = c(ll,ul),direction = -1)+#,
                          #labels = c('<2',3,4,5,'>6')) +
    #scale_color_viridis_c(direction=-1)+
    facet_wrap(~model) +
    labs( fill  = ' ',color = '∆ ºC') +
    ggthemes::theme_map() +
    theme(legend.position = 'right',
          legend.justification = 'center') 

g<-gridExtra::grid.arrange(p1,p2,nrow=2,heights=c(.25,.75))

#ggsave('../drb_gwnet/2_analysis/figures/annual_reach_noise_nopt.png', 
#       plot=g, width=4, height=5, units = 'in')


### Based on spatial join
dam_inf <- baseline %>% st_as_sf(., sf_column_name = 'geometry')%>%
  st_join(dams %>% select(DAM_NAME, AREA_SKM) %>% st_buffer(500)) %>%
  st_set_geometry(NULL)%>%
  #group_by(model) %>%
  #mutate(mean_diff = mean(diffs, na.rm=T)) %>%
  #ungroup() %>%
  mutate(dam = ifelse(!is.na(AREA_SKM), 'Dam', 'Not Dam')) %>%
  group_by(dam, model) %>%
  summarize(dam_inf = mean(diffs,na.rm=T)) %>%
  pivot_wider(names_from = 'dam',values_from = dam_inf) %>%
  mutate(diff_dams = (`Not Dam`-Dam)/`Not Dam`)

##### Based on Res Info
res_info <- readRDS('data_DRB/DRB_spatial/segments_relative_to_reservoirs.rds') %>%
  mutate(dam = if_else(type_res %in% c('reservoir_inlet_reach', 'contains_reservoir',"within_reservoir", "downstream of reservoir (1)","reservoir_outlet_reach"), 'Dam','Not Dam'))
dam_inf <- baseline %>%
  left_join(res_info %>% select(seg_id_nat, dam)) %>%
  #group_by(model) %>%
  #mutate(mean_diff = mean(diffs, na.rm=T)) %>%
  #ungroup() %>%
  group_by(dam, model) %>%
  summarize(dam_inf = mean(diffs,na.rm=T)) %>%
  pivot_wider(names_from = 'dam',values_from = dam_inf) %>%
  mutate(diff_dams = (`Not Dam`-Dam)/`Not Dam`)


widths <- c(-0.80629202, -0.610984  , -0.11562659, -0.09800233, -0.03252349,
-0.72965542, -0.66096282, -0.34081134, -0.7467914 ,  0.2541072 ,
-0.02720986, -0.66061435, -0.69001922,  0.08700078,  0.25053093,
0.35351023, -0.39122627, -0.30990819, -0.24570671, -0.10471443,
-0.38359143, -0.60594633,  0.15433205, -0.7171602 ,  0.35948055,
0.24459394,  0.68671125,  0.69848133,  0.73683074, -0.88116213,
-0.90977997, -0.4213187 , -0.3225633 , -0.6161986 , -0.20127457,
-0.67354593, -0.35500641, -0.26670843, -0.09529851, -1.03654763,
-0.13981812, -0.22328247, -0.13545558, -0.15277055, -0.7056121 ,
-0.09229423,  1.90577446, -0.06589172,  1.90772408,  1.95241391,
1.98331316,  1.98600709,  2.02593651, -0.09219526,  2.28516566,
2.31044083,  2.31936391, -0.67222613, -0.29954064,  2.36661401,
2.36894451, -0.70909874,  2.37518714,  2.38191487, -0.29885866,
-0.52823787, -0.30950074, -0.25738576,  2.33501972, -0.25225835,
2.35446887, -0.88919141, -0.26892875, -0.2396012 , -0.55441895,
-1.06453698, -0.89564009, -1.1182827 , -1.35716559, -1.11856713,
-0.75571613,  0.06727976,  0.15331616,  0.18517528, -0.52642469,
-0.85988692,  0.24653017, -0.60012408,  0.32057774,  0.36290072,
-0.52645987,  0.45241778,  0.32434513, -0.41987967,  1.28539007,
-0.60506272, -0.23736171,  0.49961989,  0.51703506,  1.11210448,
1.08131858, -0.45732992, -0.23705545,  1.16517175,  1.21533748,
1.22712455, -0.59989553, -0.58413839,  1.25413816,  1.2871168 ,
-0.57693742, -0.55866172, -0.5554664 , -0.47744437, -0.46229121,
-0.46168247, -0.11336254, -0.30463649,  0.48988648,  0.1147525 ,
0.05455845,  0.05332431, -0.12820787, -0.42593463, -0.35490306,
-0.03070118, -0.40164755,  0.11280509,  0.04998072, -1.05853671,
-0.9348742 , -0.57541017, -0.47937355, -0.12874128, -0.07770697,
-0.33460383,  0.08547363, -1.06422509, -0.83877013, -0.69419364,
-0.24349603, -0.60005663,  0.03937218, -0.44224616,  0.20546104,
0.30009964, -0.60137189,  0.1852971 ,  0.29997884, -0.50324087,
0.05253385,  1.53802368,  0.34479554,  1.61279889,  1.7397419 ,
-0.50306846,  1.76373296, -0.12618532,  1.80100163, -0.42613562,
-0.93048526, -0.37628803, -0.2382088 , -0.36331513, -0.54378143,
-0.16508604,  1.82041913, -0.51061589, -0.44411774, -0.5953537 ,
-0.40398427,  0.17351556,  1.82237555, -0.10077842, -0.17200971,
1.90209372,  0.20774161,  0.36037666, -0.35059571, -0.16339882,
-0.29609815, -0.60700168, -0.75412509, -0.57715262, -1.10339385,
-0.62388436, -0.35054956, -0.13585492,  0.07725255, -0.59625665,
0.15319405, -0.81919137, -0.23700926, -0.86569484, -0.56126372,
-0.45943629, -0.23697985, -0.39631519, -0.46417643, -0.70951851,
-0.36801883, -0.31756764, -0.32603598, -0.51109185,  0.2834377 ,
-0.43830189, -0.72651119,  0.48070005, -0.18697375,  0.52591351,
0.7574452 , -0.36759045,  0.69942706,  1.07350282,  0.99057651,
1.02733805, -0.43940896,  0.86592362,  0.02675886, -0.59959636,
0.02443938, -0.40004477, -0.31812628, -0.38102179,  2.40082746,
2.4486262 , -0.87035109, -0.65824956, -0.72390977, -0.69382529,
-0.31523498, -0.55458019, -0.1016762 , -0.16417245,  0.08200926,
-0.78056136,  2.48999583, -0.77954301,  2.48549491,  0.2546714 ,
-1.08675966, -0.59706948, -0.82346852,  2.53925599, -0.44879372,
-0.17136613, -0.55581408, -0.57073438, -0.50683897, -1.23146271,
-0.23098083, -0.43120762, -1.23655749, -0.41951371, -0.14329635,
-0.07869251, -0.88625128, -0.7360815 , -0.83379776, -0.49188766,
-0.84350517, -1.03798912, -1.0260394 , -0.74251806, -0.60566383,
-0.6449892 , -0.7721725 , -0.44788144,  2.55298474,  2.57150187,
2.55944633, -0.49298685, -0.56809199, -0.83521492,  2.59557045,
2.81376368, -0.54118803,  2.83197353,  2.81880047, -0.24074081,
2.83991497,  2.83595741, -0.62499379, -0.44755282, -1.02468347,
-0.54877833, -0.54121404, -0.6287093 , -0.52633347, -0.77217536,
-0.66709207, -1.15521911, -0.470235  , -0.93726313, -0.82640169,
-0.75426623, -0.3284019 , -0.53534305, -0.53119651, -1.07563695,
-0.22547578, -1.09586609, -0.2034108 , -0.71139305, -0.68157332,
-0.51320511, -0.50386073, -0.49809643, -0.53584403, -0.32195279,
-0.45731726, -0.52984051, -0.19740047, -0.98110937, -0.57826805,
-0.54933758, -0.47297696, -0.3683959 , -0.46880368, -0.48946392,
-0.77773926,  0.13841338,  0.14417547,  0.17888604,  0.06186177,
-0.41912886, -1.20569332, -1.18686458,  0.15758408,  0.17237506,
0.1865912 ,  0.20200168, -0.15161978, -0.56247202, -0.30412951,
-0.12908018,  0.27094481, -0.43203437, -0.16028189, -0.19144866,
-0.05230429, -0.79145495, -0.17458668,  0.01701567,  0.00632089,
-0.65718212, -0.75760394, -0.5530595 , -0.49489557, -0.40734875,
-0.47521913, -0.51243065,  0.12979193, -0.51107567, -0.60102016,
-0.77471048, -1.03530077,  0.04145431, -0.04741996,  0.08621831,
-0.52318183, -0.4875927 , -0.7457461 , -0.44733023, -0.46658189,
-0.60617071, -0.74990214,  0.36201772,  0.00647679,  0.36663964,
0.60369653,  0.04917712,  0.77957005, -0.37555274,  0.87927768,
0.93238696,  1.22031517,  0.96889023, -0.425188  ,  1.02878882,
0.22241299,  1.18479277, -0.74475582,  1.27519381, -0.44719108,
1.24830957, -0.5400604 , -0.33528961, -0.41903816, -0.04183875,
-1.11966992, -0.13696597, -0.66036651, -0.44298724,  1.28086847,
1.29543207, -0.70932405, -0.78224115, -0.53776348, -0.35712168,
-0.31390767, -0.34427585, -0.96995816, -0.51893947, -1.21288242,
-1.08083121, -0.91122533, -0.27062524, -0.40485773, -0.42005585,
-0.5771013 , -0.91878859, -0.80493141, -0.91804991, -0.71259103,
-0.88708967, -0.52942657, -0.31891117, -0.86605916, -0.49252668,
-0.6946861 , -0.06017177, -0.03494505, -0.24747783, -0.53624868,
-0.33864238,  0.25164374,  0.26928168,  0.49038353, -0.57104765,
-0.80286254, -0.75492644,  2.92198804,  2.86578398, -0.97906104,
2.86467154, -0.31865281,  2.92813236, -0.24747816,  2.94297634,
-0.5009395 , -0.56994733,  2.954492  ,  2.9502482 ,  2.96070113,
-1.10502625, -0.54917247,  2.96783135,  2.97440761,  2.97801357,
-0.23986075,  2.99165911,  2.84963203, -0.53491283, -0.68596235)

seg_ids <- c(1435, 1436, 1437, 1438, 1439, 1440, 1441, 1442, 1443, 1444, 1445,
             1446, 1447, 1448, 1449, 1450, 1451, 1452, 1453, 1454, 1455, 1456,
             1457, 1458, 1459, 1460, 1461, 1462, 1463, 1464, 1465, 1466, 1467,
             1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475, 1476, 1477, 1478,
             1479, 1480, 1481, 1482, 1483, 1484, 1485, 1486, 1487, 1488, 1489,
             1490, 1491, 1492, 1493, 1494, 1495, 1496, 1497, 1498, 1499, 1500,
             1501, 1502, 1503, 1504, 1505, 1545, 1546, 1547, 1548, 1549, 1550,
             1551, 1552, 1553, 1554, 1555, 1556, 1557, 1558, 1559, 1560, 1561,
             1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570, 1571, 1572,
             1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581, 1582, 1583,
             1584, 1585, 1586, 1587, 1588, 1589, 1590, 1591, 1592, 1593, 1594,
             1595, 1596, 1597, 1598, 1599, 1600, 1601, 1602, 1634, 1635, 1636,
             1637, 1638, 1639, 1640, 1641, 1642, 1643, 1644, 1645, 1646, 1647,
             1648, 1649, 1650, 1651, 1652, 1653, 1654, 1655, 1656, 1657, 1658,
             1659, 1660, 1661, 1662, 1663, 1664, 1665, 1666, 1667, 1668, 1669,
             1670, 1671, 1672, 1673, 1674, 1675, 1676, 1677, 1678, 1679, 1680,
             1681, 1682, 1683, 1684, 1685, 1686, 1687, 1688, 1689, 1690, 1691,
             1692, 1693, 1694, 1695, 1696, 1697, 1698, 1699, 1700, 1701, 1702,
             1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1713,
             1714, 1715, 1716, 1717, 1718, 1719, 1720, 1721, 1722, 1723, 1724,
             1762, 1763, 1764, 1765, 1766, 1767, 1768, 1769, 1770, 1771, 1772,
             1773, 1774, 1775, 1776, 1777, 1778, 1779, 1780, 1781, 1782, 1783,
             1784, 1785, 1786, 1787, 1788, 1789, 1790, 1791, 1792, 1793, 1794,
             1795, 1796, 1797, 1798, 1799, 1800, 1801, 1802, 1803, 1804, 1805,
             1806, 1807, 1808, 1809, 1810, 1811, 1812, 1813, 1814, 1815, 1816,
             1817, 1818, 1819, 1820, 1821, 1822, 1823, 1824, 1825, 1826, 1827,
             1828, 1829, 1830, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
             2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
             2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033, 2034,
             2035, 2036, 2037, 2038, 2039, 2040, 2041, 2042, 2043, 2044, 2045,
             2046, 2047, 2048, 2277, 2278, 2279, 2280, 2281, 2282, 2283, 2284,
             2285, 2286, 2287, 2288, 2289, 2290, 2291, 2292, 2293, 2294, 2295,
             2296, 2297, 2298, 2299, 2300, 2301, 2302, 2303, 2304, 2305, 2306,
             2307, 2308, 2309, 2310, 2311, 2312, 2313, 2314, 2315, 2316, 2317,
             2318, 2319, 2320, 2321, 2322, 2323, 2324, 2325, 2326, 2327, 2328,
             2329, 2330, 2331, 2332, 2333, 2334, 2335, 2336, 2337, 2338, 2339,
             2686, 2687, 2688, 2689, 2690, 2691, 2692, 2693, 2694, 2695, 2696,
             2697, 2698, 2699, 2700, 3557, 3559, 3560, 3561, 3562, 3563, 3564,
             3565, 3566, 3567, 3568, 3569, 3570, 3571, 3572, 3573, 3574, 4182,
             4183, 4184, 4185, 4186, 4187, 4188, 4189, 4190, 4191, 4192, 4193,
             4194, 4195, 4196, 4197, 4198, 4199, 4200, 4201, 4202, 4203, 4204,
             4205, 4206, 4230, 4231)

widths <- tibble(width = widths, seg_id_nat = seg_ids) 

widths %>% left_join(baseline) %>%
  ggplot(., aes(x=width, y=diffs)) +
  geom_point() +
  facet_wrap(~model)

corr <- widths %>% left_join(baseline) %>%
  group_by(model) %>%
  summarise(corr = cor(width,diffs))




