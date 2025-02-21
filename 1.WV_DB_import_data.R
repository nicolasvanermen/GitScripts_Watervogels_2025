library(DBI)
### A database interface definition for communication between R and relational database management systems. 
library(inbodb)
### Read data from INBO databases (SQL Server) with R - see https://tutorials.inbo.be/tutorials/r_database_access/
library(tidyverse)

################################################
# 1 INLEZEN VAN DATA UIT DE WATERVOGELDATABANK
################################################

con <- inbodb::connect_inbo_dbase("W0004_00_Waterbirds")

WV_DB <- dbGetQuery(con,
                         "SELECT
                         
                         SU.SurveyCode as ProjectCode
                         , SU.SurveyNaam as Project
                         , L.RegioWVCode as RegioCode
                         , L.RegioWVNaam as Regio
                         , L.LocationWVCode as GebiedsCode
                         , L.LocationWVNaam as Gebied
                         , SS.Seasonname as Telseizoen
                         , E.EventCode as TellingCode
                         , E.EventLabel as Telling
                         , E.SortOrder as TellingSortOrder
                         , S.sampleDate as Teldatum
                         , S.CoverageCode as Telvolledigheid
                         , T.Commonname as NedNaam
                         , T.scientificname as WetNaam
                         , F.Taxoncount as Aantal
                         
                         FROM FactAnalyseSetOccurrence F
                         
                         inner join DimSurvey SU on SU.surveykey = F.surveykey
                         inner join DimLocationWV L on L.locationwvkey = F.locationwvkey
                         inner join DimSeason SS on SS.Seasonkey = F.seasonkey
                         inner join DimEvent E on E.eventkey = F.eventkey
                         inner join DimSample S on F.samplekey = S.samplekey
                         inner join DimTaxonWV T on T.taxonwvkey = F.taxonwvkey
                         
                         WHERE 1=1
                           AND S.samplestatus = 'CHECKED' /**alleen gevalideerde records**/
                           AND S.coveragecode not in ('-', 'N')  /**niet getelde tellingen zijn irrelevant**/
                           AND F.taxoncount > 0
                           AND F.Analysesetkey in (2, 3, 4) /**alleen midmaandelijkse tellingen, i.e. uit MIDMA-analysesets**/")

WVgroep <- dbGetQuery(con,
                      "SELECT
                         TW.commonname as NedNaam
                        ,TW.TaxonGroupCode as Groepscode
                        ,TW.TaxonGroupDescription as Groep
                         FROM DimTaxonWV TW")

dbDisconnect(con)

### beetje verkenning:
WVgroep %>% glimpse()
WVgroep %>% distinct(Groep)
WVgroep %>% filter(Groep == "-")

WV_DB %>% glimpse()
### ter controle: 964,294 rijen (23-01-2025) - 40 extra rijen op 21-02-2025

WV_DB %>% distinct(ProjectCode, Project)
WV_DB %>% group_by(Project) %>% summarise(N = n()) 
### dit kan ook als: WV_DB %>% summarise(N = n(), .by = Project) 

### ter controle: 
# 1 Ganzentellingen Oostkustpolders          5122
# 2 HVP-tellingen steltlopers Vlaamse kust   4713
# 3 Midmaandelijkse Watervogeltellingen    877085 (877125 op 21-02-2025)
# 4 Slaapplaatstellingen Aalscholvers           2
# 5 Watervogeltellingen Zeeschelde          77372

### join met WVgroep
WV_DB <- WV_DB %>% left_join(WVgroep)

### check of er soorten zijn met onbekende groep
WV_DB %>% filter(Groep == "-") %>% glimpse()
WV_DB %>% filter(Groep == "Zwanen") %>% glimpse()

### export
# write.csv(WV_DB, paste("./Data/WV_DB_", Sys.Date(), ".csv", sep = ""))
