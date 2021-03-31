using DataFrames, CSV, Statistics, Dates

function convertdays2weeks(x)
    return Int.(round.(x/7, RoundUp))
end

function convertdays23weeks(x)
    return Int.(round.(x/21, RoundUp))
end

cd(joinpath(ENV["HOME"], "McKay_etal_2021", "Fig4"))
datadir = "yourdatafolder"
raw = DataFrame(CSV.File(joinpath(datadir, "Source Data 11.csv")))

# make sure all missing observed are unobserved
df[ismissing.(df[:Observed]), :Observed] = 0

df[:DeathDate] = map(p -> Dates.Date(p, "m/dd/yy") + Year(2000), df[:DeathDate])

# and calculate lifespan
# put in temp hatch date for some:
df[:HatchDate] = map(p -> Dates.Date(p, "m/dd/yy") + Year(2000), df[:HatchDate])

df[:Lifespan_days] = df[:DeathDate] .- df[:HatchDate]
df[:Lifespan_days] = map(p -> p.value, df[:Lifespan_days])

#remove Array{Missing,1} from df
df = df[:,map(p -> typeof(df[p]) != Array{Missing,1}, names(df))]

df[:Lifespan_weeks] = convertdays2weeks(df[:Lifespan_days])
df[:Lifespan_3weeks] = convertdays23weeks(df[:Lifespan_days])

# export for lifespan alone
CSV.write("firstlifespandata.csv", df)
