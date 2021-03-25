using DelimitedFiles, Plots, Dates

cd(joinpath(ENV["HOME"], "Dropbox", "McKay_CodeChecking", "Code", "Fig4"))

function createhazardrate(df, win)
    headers = df[2]
    df = df[1]
    # get rid of censored
    ObservedIndex = findall(p -> p == "Observed", headers)[1][2] + 1
    WeeksIndex = findall(p -> p == "Lifespan_3weeks", headers)[1][2] + 1
    df = df[df[:,ObservedIndex] .==1,:]
    lifespans = df[:,WeeksIndex]
    hz = []
    for t in 1:win:maximum(df[:,WeeksIndex])+1
        # deaths in the time window
        Dx = count(p -> t <= p < t+win, lifespans)
        #Dx = count(p -> t < p <= t+win, lifespans)
        # individuals at risk
        Nx = count(p -> p >= t, lifespans)
        #Nx = count(p -> p > t, lifespans)
        # hazard function
        hzr = Dx/Nx
        push!(hz, [t hzr])
    end
    hz = vcat(hz...)
    # hazard rate
    #hz[:,2] = -log.(1 .- hz[:,2])
    # LN of hazard rate?
  #  hz[:,2] = log.(hz[:,2])

    return hz
end

function plotgompertz(hz, IMR, RoA)
    p =scatter(hz[:,1], hz[:,2], leg = false)
    f(x) = log(IMR) + x*(RoA)
    plot!(f, 0:maximum(hz[:,1]), leg = false)
    return p
end

function plotgompertz(hz, IMR, RoA, label, hz2, IMR2, RoA2, label2)
    p =scatter(hz[:,1], hz[:,2], label = label, color = "red")
    scatter!(p, hz2[:,1], hz2[:,2], label = label2, color = "blue")
    f(x) = (RoA)*x + log(IMR)
    plot!(f, 0:maximum(hz[:,1]),  label = label, color = "red")
    g(x) = (RoA2)*x + log(IMR2)
    plot!(g, 0:maximum(hz2[:,1]),  label = label2, color = "blue")
    return p
end

function lnhaz(hz)
    hz = hz[hz[:,2] .!= Inf, :]
    hz = hz[hz[:,2] .!= -Inf, :]
    hz = hz[hz[:,2] .!= 0, :]
    hz = hz[.!isnan.(hz[:,2]), :]
    hz[:,2] = log.(hz[:,2])
    return hz
end

function convertplotlife(df, win, IMR, RoA)
    hz = createhazardrate(df, win)
    hz= lnhaz(hz)
    plotgompertz(hz, IMR, RoA)
end

function convertplotlife(df1, IMR1, RoA1, label1, df2, IMR2, RoA2, label2,  win)
    hz1 = createhazardrate(df1, win)
    hz2 = createhazardrate(df2, win)
    hz1= lnhaz(hz1)
    hz2= lnhaz(hz2)
    return plotgompertz(hz1, IMR1, RoA1, label1, hz2, IMR2, RoA2, label2)
end

##################################################################

##################################################################
# male (after processing with GompertzCalcs.R)
##################################################################
#
dfmal = readdlm("male_al.csv", ',',header =true)
dfmdr = readdlm("male_dr.csv", ',', header =true)

#hzdfmal = lnhaz(createhazardrate(dfmal, 1))
#scatter(hzdfmal[:,1], hzdfmal[:,2])

#hzdfmdr = lnhaz(createhazardrate(dfmdr, 1))
#scatter!(hzdfmdr[:,1], hzdfmdr[:,2])

# These values come from Fig4.R
shape =0.3388
rate =0.0385
IMRal = rate
RoAal = shape

#convertplotlife(dfmal, 1, IMRal, RoAal)

# These values come from Fig4.R
shape=0.1457
rate =0.0557
IMRdr = rate
RoAdr = shape

#convertplotlife(dfmdr, 1, IMRdr, RoAdr)

# NOTE: these are in 3 week intervals
p = convertplotlife(dfmal, IMRal, RoAal, "AL", dfmdr, IMRdr, RoAdr, "DR", 1)
dirout = "KillifishFeederPaper_AndrewMcKay"
savefig(p, joinpath(ENV["HOME"], "Dropbox/$dirout/Figures/Fig4/secondcohortMales_ALvsDRGompertz_flexsurvreg.pdf"))
