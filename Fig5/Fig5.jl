##############################################################
## Load packages, setup paths
#############################################################
# need to be installed
# might need to update to latest JuliaDB package by running  `]add JuliaDB@0.13.1`
using Distances, Statistics, StatsBase, StatsPlots, Interpolations, JuliaDB, CSV, Clustering, DataFrames
# should already be there
using DelimitedFiles, Dates
#setup environment
projectdir = joinpath(ENV["HOME"], "McKay_etal_2021")
datadir = "yourdatadir"
plotsout = joinpath(projectdir, "Fig5")
metadatafile = joinpath(datadir, "Source Data 12.csv")
mdata = readdlm(metadatafile, ',', header=true)
include(joinpath(projectdir, "Fig5",  "fxnsFig5.jl"))
formdir = joinpath(datadir, "Training Trajectories")

# building JuliaDB file from csv trajectories and saving
dbraw = loadtable(formdir)
JuliaDB.save(dbraw, joinpath(datadir, "TrajectoriesDB"))

# can also load pre-built db after saving
#dbraw = JuliaDB.load(joinpath(datadir, "TrajectoriesDB"))
############################################################

#############################################################
## Negative controls
############################################################
# pulling negative control (red light, no food)
# f7 and f8 had defective feeders, so no food dropped with the light, as well as f10, f11, and f12
redfneg = [7,8,10,11, 27] # f12 is double neg (no light)
doublefneg = [12, 18 ] # f12 is double neg (no light)
#############################################################

#############################################################
## Initial processing
#############################################################
# remove vids with too low a max likelihood for head
db = removelowlikelihood(dbraw, 0.99)
# find distance traveled during light turning on, normalized to initial position
mets =JuliaDB.groupby(catchlightdistance, db, (:fishnum, :recordingnum), select=(:bodyparts_coords, :fishhead_x, :fishhead_y, :fishhead_likelihood, :fishnum, :recordingnum, :redlight_x, :redlight_y, :redlight_likelihood))
mets = JuliaDB.rename(mets, 1 => :fishnum, 2 => :recordingnum, 3 => :catchlightdistance)
# filter out those without coordinates
fnt = filter(p -> p.catchlightdistance[1] != 0, mets)
#############################################################

############################################################
## Calculate autoscores and load manual scores for comparison
############################################################
thresh = 0.5 # for distclosed
negplots, posplots, autoscores = getscores(fnt, thresh, plotsout, redfneg, doublefneg)
#
autoscores = vcat(autoscores...)
#
# plot out compared to manual scoring
ages = mdata[1][:,(mdata[2] .== "age")[:]]
manscores = mdata[1][:,(mdata[2] .== "Time to learning")[:]]
agecats = mdata[1][:,(mdata[2] .== "AgeCat")[:]]
manscores = hcat(collect(1:27), manscores)
#
# compare to automated scoring
#autoscores = sortslices(scores, dims=(1,2))
allscores = hcat(manscores, autoscores[:,2], ages)
allscores = allscores[allscores[:,2] .!= "NA", :]
allscores = allscores[allscores[:,3] .!= "NA", :]

#############################################################
## Save out data for pltting in R (Fig C, D, and F)
############################################################
# making plot with young and old using inverse learning index
header = ["fishnum" "man" "auto" "agedays" "manindex" "autoindex" "label"]
allscoreslab = hcat(allscores, 1 ./ allscores[:,2], 1 ./ allscores[:,3])
allscat = []
for i in allscores[:,1]
				push!(allscat, agecats[i])
end
allscoreslab = hcat(allscoreslab, allscat)
# writeout
allscoreslab = vcat(header, allscoreslab)
writedlm(joinpath(datadir, "allscoreslab.csv"), allscoreslab, ',')
############################################################

#############################################################
## Get data for compass plots
############################################################
x = filter(p -> p.fishnum == 3, mets)
vs = getvelocity(JuliaDB.select(x, :catchlightdistance))
vdeg = conv2degrees(vs)
initialtrainings = binvecs(vdeg[1:5, :], 12)
latertrainings = binvecs(vdeg[14:19, :], 12)
writedlm(joinpath(datadir, "bins_first5_f3.csv"),initialtrainings , ',')
writedlm(joinpath(datadir, "bins_14to19_f3.csv"),latertrainings , ',')
############################################################

#############################################################
## generating examples for fish 1
#############################################################
f = 1
x = filter(p -> p.fishnum == f, fnt)
x = JuliaDB.select(x, :catchlightdistance)
dimsxy = (-550,550)
plot(0, 0,  xlims =dimsxy, ylims = dimsxy, legend = false, xlabel = "", ylabel = "", ticks = nothing, border = :none)
for i in 1:3
				t = x[i][1]
				plot!(t[:,2], t[:,1],  xlims =dimsxy, ylims = dimsxy, legend = false, xlabel = "", ylabel = "", ticks = nothing, border = :none)
end
hline!([0], line = (1, :dash, 0.4, [:grey]))
vline!([0], line = (1, :dash, 0.4, [:grey]))
savefig(joinpath(plotsout, "fish1example_1to3.pdf"))
plot(0, 0,  xlims =dimsxy, ylims = dimsxy, legend = false, xlabel = "", ylabel = "", ticks = nothing, border = :none)
for i in 14:16
				t = x[i][1]
				plot!(t[:,2], t[:,1],  xlims =dimsxy, ylims = dimsxy, legend = false, xlabel = "", ylabel = "", ticks = nothing, border = :none)
end
hline!([0], line = (1, :dash, 0.4, [:grey]))
vline!([0], line = (1, :dash, 0.4, [:grey]))
savefig(joinpath(plotsout, "fish1example_14to16.pdf"))
#############################################################

##############################################################
## Creating plots for initial location control
#############################################################
## pull initial locations of animals, will catch videos
initlocale(dbraw, mdata, plotsout)
#############################################################


##############################################################
## Creating plots for startle response
#############################################################
# separate out metadata
# first, remove redlight not turning on
smdata = mdata[1][(mdata[1][:, (mdata[2] .== "RedLight")[:]] .== 1)[:], :]
# then remove blanks for startle response
smdata = smdata[(smdata[:, (mdata[2] .== "startleResponse")[:]] .!= "")[:], :]
# then remove NAs for startle response
smdata = smdata[(smdata[:, (mdata[2] .== "startleResponse")[:]] .!= "NA")[:], :]
#
# then split into young and old
smdata = smdata[(smdata[:,(mdata[2] .== "AgeCat")[:]] .!= "middle")[:], :]
startleResponse = smdata[:,(mdata[2] .== "startleResponse")[:] .| (mdata[2] .== "VidWhereStartled")[:] .| (mdata[2] .== "AgeCat")[:] ]
startleResponse = vcat(["startleResponse" "VidWhereStartled" "AgeCat"], startleResponse)
writedlm(joinpath(datadir, "filteredstartle.csv"), startleResponse, ',')
#############################################################


##############################################################
## Creating plots for movement
#############################################################
# separate out metadata
# first, remove redlight not turning on
fmdata = mdata[1][(mdata[1][:, (mdata[2] .== "FoodDrop")[:]] .== 1)[:], :]
# then remove blanks for startle response
fmdata = fmdata[(fmdata[:, (mdata[2] .== "Came2topwithfood")[:]] .!= "")[:], :]
# then remove NAs for startle response
fmdata = fmdata[(fmdata[:, (mdata[2] .== "Came2topwithfood")[:]] .!= "NA")[:], :]
#
# then split into young and old
fmdata = fmdata[(fmdata[:,(mdata[2] .== "AgeCat")[:]] .!= "middle")[:], :]
came2top = fmdata[:,(mdata[2] .== "Came2topwithfood")[:] .| (mdata[2] .== "VidWhereCame2Top")[:] .| (mdata[2] .== "AgeCat")[:]]
came2top = vcat(["Came2topwithfood" "VidWhereCame2Top" "AgeCat"], came2top)
writedlm(joinpath(datadir, "filteredcame2top.csv"), came2top, ',')
#############################################################
#

##############################################################
## create aggregated compass plots
#############################################################
thresh0 = 1
thresh1 = 3
thresh2 = 5
yless, ymore, oless, omore=getcompassplots(fnt, thresh0, thresh1, thresh2, datadir, mdata, redfneg, doublefneg)
#############################################################
