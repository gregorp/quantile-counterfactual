# I went through this and tried to standardize formatting to make it 
# more readable and add in more comments.
# Comments from the original are left in, and start with a double-hash ##.
# My comments start with single-hash #.
# Please feel free to add even more comments!
#   - Gregor

library(quantreg) # needed for rq quantile regression function

## assume all data are in a data.frame called alls.d with the variables
## defined and non-missing:

ff  <- formula(dpi ~ AgeHeadQR + FamilyStructure + EducationHead + hhsize + children)
ff0 <- formula(dpi ~ AgeHeadQR + FamilyStructure + EducationHead)
## no, do all quants in every regression
## these.quants <- list(1 = 1:25, 2 = 26:50, 3 = 51:75, 4 = 75:99)
## these.quants <- 1:99
these.quants <- 1:99/100
these.quants <- c(.005, these.quants, .995)

## drop Australia
these.countries <- lis.countries[lis.countries !=  "au"]
t.ds <- these.countries

# This function re-cleans the data, selects a single country
# and runs a quantile regression on it.
# I renamed the y argument to country_acronym.
t.f <- function(country_acronym) {
    td <- subset(alls.d,
                 subset =
                     lis.acronym == country_acronym & 
                     !is.na(dpi) &
                     !is.na(AgeHeadQR) &
                     !is.na(FamilyStructure) &
                     !is.na(EducationHead) & 
                     agehead < 70)
    rq(ff,
       tau = -1,
       data = td, weights = regweight)
}

# This puts all the regression results for each individual countyr 
# in a list named qdist
qdist <- vector(mode = "list", length = length(these.countries))
names(qdist) <- these.countries
for(i in these.countries) qdist[[i]] <- t.f(i)

# This function runs CountryC's model on CountyX's data
# and takes weighted means of the results.
t.f <- function(countryC, countryX = countryC) {
    tdf <- model.frame(qdist[[countryX]])
    tw <- tdf[["(weights)"]]
    tdm <- model.matrix(qdist[[countryX]], data = tdf)
    tdq <- predict(qdist[[countryC]], newdata = model.frame(qdist[[countryX]]))
    mtdq <- sapply(1:ncol(tdq), function(x) weighted.mean(tdq[,x], tw))
    mtdq
}

# This function runs CountryC's model on the US data
t1.f <- function(countryC) {
    t.f(countryC, countryX = "us")
}

# This function runs the US's model on CountryX's data
t2.f <- function(countryX) {
    t.f(countryC = "us", countryX)
}

# cfq1 is a list of [whatever t.f outputs] for countries with themselves
cfq1 <- lapply(these.countries, function(x) t.f(x, x))

# cfq1 is a list of [whatever t.f outputs] for each country on US data
cfq2 <- lapply(these.countries, t1.f)
## for(i in these.countries) print(c(i, t1.f(i)))

# cfq1 is a list of [whatever t.f outputs] for the US on each country's data
cfq3 <- lapply(these.countries, t2.f)
names(cfq1) <- names(cfq2) <- names(cfq3) <- these.countries

## now for poverty

decomp.labels <-
    c("Actual", "Country coef, US Xs", "US coef, country Xs")

mc <- floor(length(these.quants))

poverty.rel <-
    do.call("rbind",
            lapply(these.countries,
                   function(x)
                       rbind(data.frame(poverty.rel = sum(cfq1[[x]] < .5 * cfq1[[x]][mc]),
                                        dist = decomp.labels[1],
                                        lis.acronym = x),
                             data.frame(poverty.rel = sum(cfq2[[x]] < .5 * cfq2[[x]][mc]),
                                        dist = decomp.labels[2],
                                        lis.acronym = x),
                             data.frame(poverty.rel = sum(cfq3[[x]] < .5 * cfq3[[x]][mc]),
                                        dist = decomp.labels[3],
                                        lis.acronym = x))))
tdp <-
    do.call("rbind",
            lapply(these.countries,
                   function(x)
                       rbind(data.frame(poverty.rel = sum(cfq2[[x]] < .5 * cfq2[[x]][mc]),
                                        dist = decomp.labels[2],
                                        lis.acronym = x),
                             data.frame(poverty.rel = sum(cfq3[[x]] < .5*cfq3[[x]][mc]),
                                        dist = decomp.labels[3],
                                        lis.acronym = x))))
td <- do.call("rbind",
              lapply(these.countries,
                     function(x)
                         data.frame(est.poverty.rel = sum(cfq1[[x]] < .5*cfq1[[x]][mc]),
                                    lis.acronym = x)))
tdp <- merge(tdp, td, by = "lis.acronym")
tdp$poverty.rel.difference <- tdp$poverty.rel - tdp$est.poverty.rel
tdp <- merge(tdp, country.key, by = "lis.acronym")
tdp$country <-
    ifelse(tdp$lis.acronym %in% c("uk"),
           "UK",
           ifelse(tdp$lis.acronym %in% c("us"),
                  "US",
                  ifelse(tdp$lis.acronym %in% c("cz"),
                         "Czech Rep",
                         tdp$country.name)))
countrygr.d <- countrygr.d[order(countrygr.d$CountryGroup, countrygr.d$lis.acronym),]
tmp.d <- countrygr.d[-grep("average", countrygr.d$lis.acronym),]
tmp.d$this.order <- seq(along = tmp.d$lis.acronym)
tdp <- merge(tdp, tmp.d, by = "lis.acronym")

# barchart of relative poverty differences by country
trellis.device(pdf, file = "example-counterfactual-poverty-rel.pdf")
barchart(dist ~ poverty.rel.difference|reorder(country.name, this.order),
         groups = dist,
         data = tdp, subset = lis.acronym %in% c("br", "fi", "uk", "us"),
         xlab = "Difference to estimated relative poverty rate",
         type = "l", auto.key = list(rectangles = TRUE, points = FALSE),
         as.table = TRUE,
         panel = function(x, y, ...) {
             panel.barchart(x, y, ...)
             panel.abline(v = 0, lwd = 1.3, col = "red")
         }
)
dev.off()

# dotplot of relative poverty differences by country
trellis.device(pdf, file = "counterfactual-poverty-rel.pdf")
dotplot(dist ~ poverty.rel.difference|reorder(country, this.order), groups = dist,
        data = tdp, ##subset = lis.acronym %in% c("br", "fi", "uk", "us"),
        xlab = "Difference from country estimated relative poverty rate (percentage points)",
        ## type = "l",
        ## auto.key = list(rectangles = FALSE, points = TRUE),
        as.table = TRUE,,
        panel = function(x, y, ...) {
            panel.dotplot(x, y, ...)
            ## panel.points(x, y, ...)
            panel.text(x, y, x, cex = .8, pos = 1)
            panel.abline(v = 0, lwd = 1.3, col = "red")
        }
)
dev.off() 