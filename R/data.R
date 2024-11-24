##' Example data of BTRtools: Cliamte data
##'
##' @source A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR).[DOI:10.1093/treephys/tpae127]
##' @docType data
##' @keywords datasets
##' @name Clims
##' @usage data(Clims)
##' @format A \code{data.frame} containing 11 columns :
##' @format **Year** : Year,
##' @format **DOY** : Day of years,
##' @format **Month** : Month,
##' @format **Day** : Day,
##' @format **TEM** : Temperature,
##' @format **gE** : Grwoth rate by light,
##' @format **soilM** : Soil moisture,
##' @format **Ls** : Day length,
##' @format **rootd** : Root zone depth,
##' @format **dL_i** : dealt day length,
##' @format **VPD** : vapor Pressure Deficit.
##'
'Clims'

##' Example data of BTRtools: Parameter data.frame
##'
##' @source A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR).[DOI:10.1093/treephys/tpae127]
##' @docType data
##' @keywords datasets
##' @name BPparam
##' @usage data(BPparam)
##' @format A \code{data.frame} containing 7 columns :
##' @format **Parameter** : is the parameter name in Rcode,
##' @format **ParamType** : is the parameter type,
##' @format **Module** : is the module running the parameter,
##' @format **Note** : is the object for which the parameter is used,
##' @format **Description** : is the description of parameter,##'
##' @format **Values** : is the parameter value.
##' @format **Unit** : is the unit parameter value.
##'
'BPparam'

##' Example data of BTRtools: observed value of tree ring
##'
##' @source A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR).[DOI:10.1093/treephys/tpae127]
##' @docType data
##' @keywords datasets
##' @name BPring
##' @usage data(BPring)
##' @format A \code{data.frame} containing 5 columns :
##' @format **Year** : Year,
##' @format **MRW** : Mean ring width,
##' @format **MaxLA** : Maxium lumen area in ring,
##' @format **CD** : Vessel density,
##' @format **RCTA** : Vessel fraction.
##'
'BPring'

##' Example data of BTRtools: observed value of fiber cells, output file of ROXAS
##'
##' @source A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR).[DOI:10.1093/treephys/tpae127]
##' @docType data
##' @keywords datasets
##' @name Cells
##' @usage data(Cells)
##' @format A \code{data.frame} containing 33 columns :
##'
'Cells'

##' Example data of BTRtools: observed value of vessels, output file of ROXAS
##'
##' @source A process-based model of climate-driven xylogenesis and tree-ring formation in broad-leaved trees (BTR).[DOI:10.1093/treephys/tpae127]
##' @docType data
##' @keywords datasets
##' @name Vessels
##' @usage data(Vessels)
##' @format A \code{data.frame} containing 33 columns :
##'
'Vessels'
