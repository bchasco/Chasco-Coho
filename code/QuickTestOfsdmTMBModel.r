dat <- read.csv(file = "C:/NOAA/LARGE_DATA/OCcoho_rearHR17new_naomit.csv")
scale_this <- function(x) as.vector(scale(x))
my_factor <- function(x) as.factor(x)

dat <- dat %>%       mutate(UTM_E_km = UTM_E/1000, #Rescale
UTM_N_km = UTM_N/1000, #Rescale
# ID_Num = ID_Num,
fYr = as.factor(year),
fSTRM_ORDER = as.factor(STRM_ORDER),
StrmPow = scale_this(StrmPow),
WidthM = scale_this(WidthM),
W3Dppt = scale_this(W3Dppt),
MWMT_Index = scale_this(MWMT_Index),
SprPpt = scale_this(SprPpt),
IP_COHO = scale_this(IP_COHO),
SolMean = scale_this(SolMean),
StrmSlope = scale_this(StrmSlope),
OUT_DIST = scale_this(OUT_DIST),
M07day_summer = scale_this(M07day_summer)
)
m1 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
M07day_summer
")
)
dat$dens <- dat$Juv.km
mesh <- make_mesh(dat, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
sdmTMB::sdmTMB(formula = m1, 
               data = dat, 
               mesh = mesh,
               time = "JuvYr",
               family = tweedie(link = "log"),
               spatial = "on",
               spatiotemporal  = "iid")

corrplot::corrplot(cor(dat[,c('OUT_DIST','M07day_summer','SprPpt','MWMT_Index','W3Dppt')]))
