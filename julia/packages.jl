# External (non-Stdlib) packages support
# To add support for new packages:
# 1. Add the package name as a Symbol in the supported_packages array
# 2. Add a define_$pkg function
# 3. That function should @eval the required display() functions
# 4. Test it

"""List of symbols of package names supported by ob-julia.

Packages already included in a session get removed from this list."""
const supported_packages = [
    :DataFrames,
    :Latexify, :LaTeXStrings]

"Call define_\$pkg function."
define_package_functions(pkg::Symbol) = (@eval $pkg)()

"Defines show methods based on packages loaded by the user in the
current session."
function OrgBabelReload()
    for pkg in supported_packages
        if isdefined(Main, pkg) && (isa(getfield(Main, pkg), Module) ||
                                    isa(getfield(Main, pkg), UnionAll))
            define_package_functions(Symbol("define_", pkg))
            # Remove loaded packages from list to prevent multiple execution
            filter!(x -> x != pkg, supported_packages)
        end
    end
end

function define_LaTeXStrings()
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           l::Main.LaTeXString; kwargs...)
        print(d.io, String(l))
    end
end

function define_Latexify()
    # Latexify outputs LaTeXStrings.LaTeXString objects, but
    # LaTeXStrings is not included into Main now.  That's why we have
    # to define this method here
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           l::Main.Latexify.LaTeXStrings.LaTeXString; kwargs...)
        print(d.io, String(l))
    end
    # We want to latexify anything we can if the output is a tex file
    @eval function display(d::ObJuliaDisplay, ::MIME"application/x-tex",
                           obj::Any; kwargs...)
        print(d.io, Main.latexify(obj))
    end
end

function define_DataFrames()
    @eval function display(d::ObJuliaDisplay, ::MIME"text/csv",
                           df::Main.DataFrame; kwargs...)
        # text/org is just a csv which org parses automatically, so we
        # can fallback to it
        display(d, MIME("text/org"), df)
    end
    @eval function display(d::ObJuliaDisplay, ::MIME"text/org",
                           df::Main.DataFrame; kwargs...)
        # We should use CSV.jl to output a DataFrame to csv.
        out = join(string.(names(df)), ',') * '\n'
        out *= join([join(x, ',') for x in eachrow(df) .|> collect],'\n')
        print(d.io, out)
    end
end
