##################################################################
# Fig3 data collection and processing (growth and fertility)
##################################################################

using DataFrames, CSV, Dates, Statistics

##################################################################
# Fig3B: growth difference for 7 times a day/AL and 3 times a day/DR
##################################################################

function convertdays2weeks(x)
    return Int.(round.(x/7, RoundUp))
end

cd(joinpath(ENV["HOME"], "McKay_etal_2021", "Fig3"))
datadir = "yourdatafolder"
df = DataFrame(CSV.File(joinpath(datadir, "Table S11.csv")))

# make sure all missing observed are unobserved
df[ismissing.(df[:Observed]), :Observed] = 0

df[:DeathDate] = map(p -> Dates.Date(p, "m/dd/yy") + Year(2000), df[:DeathDate])

# put in temp hatch date for some:
df[:HatchDate] = map(p -> Dates.Date(p, "m/dd/yy") + Year(2000), df[:HatchDate])

# and calculate lifespan
df[:Lifespan_days] = df[:DeathDate] .- df[:HatchDate]
df[:Lifespan_days] = map(p -> p.value, df[:Lifespan_days])

#remove Array{Missing,1} from df
df = df[:,map(p -> typeof(df[p]) != Array{Missing,1}, names(df))]

df[:Lifespan_weeks] = convertdays2weeks(df[:Lifespan_days])

# first make sure have body length and non-zero
df = df[.!ismissing.(df[:BodyLength_cm_1]), :]
df = df[df[:BodyLength_cm_1] .!= 0, :]
df[:GrowthRate] = df[:BodyLength_cm_1] ./ df[:Lifespan_weeks]

dfdr = deepcopy(df)

# threshold animals that died before 120 days/4 months to maintain similar growth phase and apples to apples comparison with other groups
df = df[df[:Lifespan_days] .< 121,:]

CSV.write("fig3growthrate_DR.csv", df)

##################################################################
# Fig3E: growth difference for 7 times a day/AL and 12 times a day/OE
##################################################################
df = DataFrame(CSV.File(joinpath(datadir, "Table S7.csv")))

# make sure observed and has measurement
df = df[df[:Observed] .== 1,:]
df = df[df[:BodyLength_cm] .!= 0, :]
# apples to apples
df = df[df[:Lifespan_days] .< 121,:]

df[:GrowthRate] = (df[:BodyLength_cm]) ./ (df[:Lifespan_days])
# set any growth rate under 0 to 0 (no fish should be shrinking)
df[:GrowthRate][df[:GrowthRate] .<= 0] .= 0
# making it cm/week instead of day
df[:GrowthRate]= map(p -> p*7, df[:GrowthRate])

CSV.write("fig3growthrate_OE.csv", df)

##################################################################
# Fig3C: fertility under 7 times a day/AL and 3 times a day/DR
##################################################################
df = DataFrame(CSV.File(joinpath(datadir, "Table S5.csv")))
dfmeta = DataFrame(CSV.File(joinpath(datadir, "Table S6.csv")))

# remove missing Uncrosseds
df = df[.!ismissing.(df[:Uncrossed]), :]
df = df[.!isnan.(df[:Pair]), :]
dfmeta = dfmeta[dfmeta[:Pair] .!= :NA, :]
dfmeta = dfmeta[dfmeta[:Sex] .== "f", :]

# join two tables
dfall = join(df, dfmeta, on = :Pair)

CSV.write("fig3_DRfertility.csv", dfall)

#aggregate on fish mean
dfpairs = groupby(dfall, :Pair)
dfmean = combine(dfpairs, :TotalEmbryosHarvested => mean)
dfmean = join(dfmean, dfmeta, on = :Pair)

CSV.write("fig3_DRfertility_mean.csv", dfmean)

##################################################################
# Fig3F: fertility under 7 times a day/AL and 12 times a day/OE
##################################################################
df = DataFrame(CSV.File(joinpath(datadir, "Table S8.csv")))
dfmeta = DataFrame(CSV.File(joinpath(datadir, "Table S9.csv")))

# remove missing Uncrosseds
df = df[.!ismissing.(df[:Uncrossed]), :]
df = df[.!isnan.(df[:Pair]), :]
dfmeta[:Pair]= Meta.parse.(dfmeta[:Pair])
dfmeta = dfmeta[dfmeta[:Pair] .!= :NA, :]
# only want females (to avoid double counting)
dfmeta = dfmeta[dfmeta[:Sex] .== "f", :]

# join two tables
dfall = join(df, dfmeta, on = :Pair)
dfall = dfall[ismissing.(dfall[:Remove]), :]

CSV.write("fig3_OEfertility.csv", dfall)

# aggregate on fish mean
dfpairs = groupby(dfall, :Pair)
dfmean = combine(dfpairs, :TotalEmbryosHarvested => mean)
dfmean = join(dfmean, dfmeta, on = :Pair)

CSV.write("fig3_OEfertility_mean.csv", dfmean)
