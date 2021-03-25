# this is only to take the csvs in and format them
function reformatcsv(dirin, dirout)
		if !ispath(dirout)
				mkpath(dirout)
		end
		for (r,d,fs) in walkdir(dirin)
				for f in fs
						if occursin(".csv", f)
								fpath = joinpath(r,f)
								recordingnum = match(r"_REC([0-9]+)DLC",f)[1]
								fishnum = match(r"^t([0-9]+)_", f)[1]

								df = readdlm(fpath, ',')
								header = string.(df[2,:], "_", df[3,:])
								push!(header, "fishnum")
								push!(header, "recordingnum")
								df = df[4:end, :]
								df = hcat(df, fill(fishnum, (size(df)[1], 1)))
								df = hcat(df, fill(recordingnum, (size(df)[1], 1)))
								df = vcat(permutedims(header[:,:], (2,1)), df)
								fileout = joinpath(dirout, f)
								writedlm(fileout, df, ',')
						end
				end
		end
end


# for identifying low-likelihood animals
function removelowlikelihood(db, threshold)
		maxlike = JuliaDB.groupby(maximum, db, (:fishnum, :recordingnum), select= :fishhead_likelihood)
		maxlike = filter(p -> p.maximum < threshold, maxlike)
		maxlike = hcat(JuliaDB.select(maxlike, :fishnum), JuliaDB.select(maxlike, :recordingnum))
		for i in 1:size(maxlike)[1]
				db = filter(p -> !(p.fishnum == maxlike[i,1] && p.recordingnum == maxlike[i,2]), db)
		end
		return db
end

function swapoutlowlikelihood(x)
		if x.fishhead_likelihood < 0.8
				return Missing
		else
				return x.fishhead_likelihood
		end
end

function getlightdur(d)
		try
				chunks = []
				firstochunk = 0
				endochunk = 0
				inchunk = false
				lightloc = []
				for i in 1:length(d)
						if d.redlight_likelihood[i] >= 0.5 && !inchunk
								inchunk = true
								firstochunk = i
								push!(lightloc, [d.redlight_x d.redlight_y])
						elseif d.redlight_likelihood[i] < 0.5 && inchunk
								inchunk = false
								endochunk = i -1
								push!(chunks, (firstochunk, endochunk))
						else
						end
				end
				lightloc = vcat(lightloc...)

				largestchunk = chunks[findmax(map(p -> p[2] - p[1], chunks))[2]]
				return (largestchunk, (mean(lightloc[:,1]), mean(lightloc[:, 2])))
		catch
				return []
		end
end

# function to get distance traveled during red light
# catch function for lightdistance
function catchlightdistance(d)
		try
				return lightdistance(d)
		catch
				a = unique(d.recordingnum)[1]
				b = unique(d.fishnum)[1]
				println("caught $a $b")
				return 0
		end
end

# NOTE: this is returning a trajectory with coordinates y,x
function lightdistance(d)
		likelihoodthreshold = 0.999
		# first, get red light period of time
		chunks = []
		firstochunk = 0
		endochunk = 0
		inchunk = false
		for i in 1:length(d)
				if d.redlight_likelihood[i] >= 0.5 && !inchunk
						inchunk = true
						firstochunk = i
				elseif d.redlight_likelihood[i] < 0.5 && inchunk
						inchunk = false
						endochunk = i -1
						push!(chunks, (firstochunk, endochunk))
				else
				end
		end

		largestchunk = chunks[findmax(map(p -> p[2] - p[1], chunks))[2]]

		# constructing interpolation

		d = filter(p -> p.fishhead_likelihood > likelihoodthreshold, d)

		d = filter(p -> (p.bodyparts_coords > largestchunk[1]) && (p.bodyparts_coords < largestchunk[2]), d)

		# reversed coordinates due to horizonal orientation of videos
		rlx = mode(d.redlight_x) -55#-55 correction for water line
		rly = mode(d.redlight_y)
		RLLocale = [rlx; rly]

		treal = d.bodyparts_coords 
		tfake = 1:length(treal)
		x = d.fishhead_x 
		y = d.fishhead_y
		A = hcat(x,y)
		itp = scale(interpolate(A, (BSpline(Cubic(Natural(OnGrid()))), NoInterp())), tfake, 1:2)

		# put together array with coordinates given likelihood
		# populating first gap (none between 0 and first time point)
		gaps = []
		coords = d.bodyparts_coords .+ 1
		for i in 2:length(coords)
				push!(gaps, coords[i] - coords[i-1])
		end

		t = []
		v = 1
		maxtfake = maximum(tfake)
		for g in gaps
				for i in 1:g
						v += 1/g
						if v > maxtfake
								v = round(v)
						end
						push!(t, [itp(v, 1) itp(v,2)])
				end
		end
		t = vcat(t...)

		# calculate distance and speed based upon interpolation
		# estimate: 30 fps, so first three seconds is first 90 frames
		finaltime = 90 # should be ~3 seconds
		totaldist = 0
		maxspeed = 0
		direction = 0
		startdist = euclidean(t[1,:], RLLocale)
		enddist = euclidean(t[finaltime,:], RLLocale)
		distclosed = (startdist - enddist)/startdist
		distclosed = distclosed < -1 ? -1 : distclosed

		# normalizing for starting location
		nt = []
		t0 = t[1,:]
		push!(nt, t0 -  t0)

		for i in 2:finaltime
				push!(nt, t[i,:] - t0)
		end
		nt = map(p-> permutedims(p[:,:], (2,1)), nt)
		nt = vcat(nt...)
		return (nt, distclosed, enddist)
end

# main plotout function, pulls trajs and creates metric based upon
function plotout(trajs)
		dimsxy = (-450,450)
		ps = []
		dss = []
		distcloseds = []
		for i in 1:length(trajs)
				t = trajs[i][1]
				distclosed = trajs[i][2]
				push!(distcloseds, distclosed)
				ds = calcdirspeed(t)
				push!(dss, ds)
				ds = sprint(show, ds; context = :compact => true)
				distclosed = sprint(show, distclosed; context = :compact => true)
				title = "$distclosed"
				p = plot(t[:,2], t[:,1],  xlims =dimsxy, ylims = dimsxy, legend = false, xlabel = "", ylabel = "", ticks = nothing, border = :none, title= title, titlefont = (10, "helvetica"))
				hline!([0], line = (1, :dash, 0.4, [:grey]))
				vline!([0], line = (1, :dash, 0.4, [:grey]))
				push!(ps, p)
		end
		pl = plot(ps...)
		return pl, dss, distcloseds
end

function calcdirspeed(t)
		# ys are in first column, fyi
		d = []
		for i in 2:size(t)[1]
				ydiff = t[i,1] - t[i-1, 1]
				push!(d, ydiff)
		end
		velocityup = mean(d)
end

function vel(a, b)
		v = b .- a
		return permutedims(v[:,:], (2,1))
end

function getvelocity(trajs)
		vs = []
		for ts in trajs
				t4 = []
				for i in 4:45
						t = ts[1]
						push!(t4, vel(t[i-3,:], t[i, :]))
				end
				t4 = vcat(t4...)
				# need to swap coor to get proper orientation
				x = mean(t4[:,2])
				y = mean(t4[:,1])
				push!(vs, [x y])
		end
		vs = vcat(vs...)
		return vs
end
#
function conv2degrees(a)
		b = similar(a)
		for i in 1:size(a)[1]
				# 57.296 is constanct to convert radians to degrees
				# Sets 90 at the top
				deg = (atan(a[i,1], a[i,2]) * 57.296) + 90
				v = euclidean(a[i, :], [0,0])
				b[i, 1] = v
				b[i, 2] = deg
		end
		return b
end

# binning, assuming v, deg array
function binvecs(a, bins)
		binsize = 360/bins
		b = []
		for i in 1:bins
				binstart = (i-1)*binsize
				binend = i*binsize

				# subset to bin region
				x = a[(a[:,2] .>= binstart),: ]
				x = x[(x[:,2] .< binend),: ]
				s = sum(x[:,1])

				push!(b, [binstart s])
		end
		b = vcat(b...)
		b[:,2] = b[:,2] ./ size(a)[1]
		return b
end

function initiallocale(d)
		likelihoodthreshold = 0.999
		# first, get red light period of time
		chunks = []
		firstochunk = 0
		endochunk = 0
		inchunk = false
		for i in 1:length(d)
				if d.redlight_likelihood[i] >= 0.5 && !inchunk
						inchunk = true
						firstochunk = i
				elseif d.redlight_likelihood[i] < 0.5 && inchunk
						inchunk = false
						endochunk = i -1
						push!(chunks, (firstochunk, endochunk))
				else
				end
		end

		largestchunk = chunks[findmax(map(p -> p[2] - p[1], chunks))[2]]

		# constructing interpolation
		d = filter(p -> p.fishhead_likelihood > likelihoodthreshold, d)
		d = filter(p -> (p.bodyparts_coords > largestchunk[1]) && (p.bodyparts_coords < largestchunk[2]), d)

		rlx = mode(d.redlight_x)
		rly = mode(d.redlight_y)
		RLLocale = [rlx; rly]


		treal = d.bodyparts_coords 
		tfake = 1:length(treal)
		x = d.fishhead_x 
		y = d.fishhead_y
		A = hcat(x,y)
		itp = scale(interpolate(A, (BSpline(Cubic(Natural(OnGrid()))), NoInterp())), tfake, 1:2)

		# put together array with coordinates given likelihood
		# populating first gap (none between 0 and first time point)
		gaps = []
		coords = d.bodyparts_coords .+ 1
		for i in 2:length(coords)
				push!(gaps, coords[i] - coords[i-1])
		end

		t = []
		v = 1
		maxtfake = maximum(tfake)
		for g in gaps
				for i in 1:g
						v += 1/g
						if v > maxtfake
								v = round(v)
						end
						push!(t, [itp(v, 1) itp(v,2)])
				end
		end
		t = vcat(t...)
		return t
end

# catch function for lightdistance
function catchinitiallocale(d)
		try
				return initiallocale(d)
		catch
				a = unique(d.recordingnum)[1]
				b = unique(d.fishnum)[1]
				println("caught $a $b")
				return 0
		end
end

function rollavg(d, win)
		# take rolling average
		dw = []
		wt = win
		for i in 1:length(d)
				wt = i < win ? i : win
				push!(dw, mean(d[wt:i]))
		end
		return dw
end

function revrollavg(d, win)
		# forward
		ford= rollavg(d, win)

		# reverse
		rd = reverse(d)
		revd = rollavg(rd, win)

		return ford, reverse(revd)
end

function initlocale(db, mdata, plotdir)
		# identify initial location for animals when video starts
		initlocs =JuliaDB.groupby(catchinitiallocale, db, (:fishnum, :recordingnum), select=(:bodyparts_coords, :fishhead_x, :fishhead_y, :fishhead_likelihood, :fishnum, :recordingnum, :redlight_x, :redlight_y, :redlight_likelihood))
		initlocs = filter(p -> p.catchinitiallocale[1] != 0, initlocs)

		initlocs = JuliaDB.select(initlocs, (:catchinitiallocale, :fishnum))
		# integrating information on age
		agecats = mdata[1][:,(mdata[2] .== "AgeCat")[:]]
		ages = mdata[1][:,(mdata[2] .== "AgeCat")[:]]
		a = []
		for r in (initlocs)
				y = r.catchinitiallocale[1,1]
				x = r.catchinitiallocale[1,2]
				f = r.fishnum
				age = ages[f]
				agecat = agecats[f]
				push!(a, [x y f age agecat])
		end
		a = vcat(a...)
		p = scatter(a[:,1], a[:,2], group = a[:,5], xaxis = font(:white, 10), yaxis = font(:white, 10))
		savefig(p, joinpath(plotdir, "initial_location.pdf"))
		aold = a[a[:,5] .== "old", :]
		ayoung = a[a[:,5] .== "young", :]
		pold = scatter(aold[:,1], aold[:,2], title = "Old animals", legend = false, xaxis = font(:white, 10), yaxis = font(:white, 10))
		pyoung = scatter(ayoung[:,1], ayoung[:,2], title = "Young animals", legend = false, xaxis = font(:white, 10), yaxis = font(:white, 10))
		poldyoung = plot(pold, pyoung)
		savefig(poldyoung, joinpath(plotdir, "sidebyside_initial_location.pdf"))

		# subsample down to same
		ssize = 40
		iold = sample(1:size(aold)[1], ssize, replace = false)
		iyoung = sample(1:size(ayoung)[1], ssize, replace = false)
		pold = scatter(aold[iold, 1], aold[iold,2], title = "Old animals", legend = false, xaxis = font(:white, 10), yaxis = font(:white, 10))
		pyoung = scatter(ayoung[iyoung, 1], ayoung[iyoung, 2], title = "Young animals", legend = false, xaxis = font(:white, 10), yaxis = font(:white, 10))
		poldyoung = plot(pold, pyoung)
		savefig(poldyoung, joinpath(plotdir, "random$ssize.sidebyside_initial_location.pdf"))
end

function getscores(fnt, thresh, plotdir, redfneg, doublefneg)
		#
		negplots = []
		posplots = []
		autoscores = []
		# loop through animals, create plots and stats
		for f in unique(JuliaDB.select(fnt, :fishnum))
				x = filter(p -> p.fishnum == f, fnt)
				x = JuliaDB.select(x, :catchlightdistance)
				println("fish $f length $(length(x))")
				if length(x) < 6
						push!(autoscores, [f "NA"])
						continue
				end
				p, dss, distclosed = plotout(x)
				pdss = scatter(dss, title = "$f")
				pdss2 = scatter(distclosed, title = "$f")
				if f in union(redfneg, doublefneg)
						push!(negplots, pdss2)
				else
						push!(posplots, pdss2)
				end
				fwdss, rwdss = revrollavg(dss, 5)
				frpw = scatter(dss)
				plot!(fwdss)
				plot!(rwdss)
				counter = 0
				sessions = length(distclosed)
				for (i,y) in enumerate(distclosed)
						y > thresh ? counter = counter +1 : counter = 0
						if counter == 2
								# NOTE: this needs to be -2 because want from beginning of 2 consecutive, not after
								sessions = i - 2
								break
						end
				end
				#
				push!(autoscores, [f sessions])
				p = scatter(1:length(distclosed), distclosed)
				hline!([thresh])
				if minimum(distclosed) < -10
						println(f)
				end
		end
		return negplots, posplots, autoscores
end

function degreeconv(f, fnt, low, high)
		x = filter(p -> p.fishnum == f && low <= p.recordingnum < high, fnt)
		vs = getvelocity(JuliaDB.select(x, :catchlightdistance))
		vdeg = conv2degrees(vs)
		return vdeg
end


function getcompassplots(fnt, thresh0, thresh1, thresh2, datadir, mdata, redfneg, doublefneg)
		# convert to base 0 recordings
		thresh0 = thresh0 -1
		thresh1 = thresh1 -1
		thresh2 = thresh2 -1

		# sep out four categories
		yless = []
		ymore = []
		oless = []
		omore = []
		agecats = mdata[1][:,(mdata[2] .== "AgeCat")[:]]
		# loop through animals, create plots and stats
		for f in unique(JuliaDB.select(fnt, :fishnum))
				if f in redfneg || f in doublefneg
				else

						try
								if agecats[f] == "young"
										vdeg = degreeconv(f, fnt, thresh0, thresh1)
										if length(vdeg) > 0
												push!(yless, vdeg)
										end
										vdeg = degreeconv(f, fnt, thresh1, thresh2)
										if length(vdeg) > 0
												push!(ymore, vdeg)
										end
								elseif agecats[f] == "old"
										println("old")
										println(f)
										vdeg = degreeconv(f, fnt, thresh0, thresh1)
										if length(vdeg) > 0
												push!(oless, vdeg)
										end
										vdeg = degreeconv(f, fnt, thresh1, thresh2)
										if length(vdeg) > 0
												push!(omore, vdeg)
										end
								end
						catch
								println("caught")
								println(f)
						end
				end
		end
		yless = vcat(yless...)
		ymore = vcat(ymore...)
		oless = vcat(oless...)
		omore = vcat(omore...)
		yless = binvecs(yless, 12)
		ymore = binvecs(ymore, 12)
		oless = binvecs(oless, 12)
		omore = binvecs(omore, 12)
		writedlm(joinpath(datadir, "young_pretrain.csv"),yless , ',')
		writedlm(joinpath(datadir, "young_posttrain.csv"),ymore , ',')
		writedlm(joinpath(datadir, "old_pretrain.csv"),oless , ',')
		writedlm(joinpath(datadir, "old_posttrain.csv"),omore , ',')
		return yless, ymore, oless, omore
end
