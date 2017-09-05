namespace Eurostat
open System
open System.IO
open FSharp.Data
open System.Collections.Generic

module Vocabulary =
  let testme = printfn "Test me"
  let euCountries = 
     ["BE", "Belgium";
         "BG", "Bulgaria";
         "CZ", "Czech Republic";
         "DK", "Denmark";
         "DE", "Germany";
         "EE", "Estonia";
         "IE", "Ireland";
         "EL", "Greece";
         "ES", "Spain";
         "FR", "France";
         "HR", "Croatia";
         "IT", "Italy";
         "CY", "Cyprus";
         "LV", "Latvia";
         "LU", "Luxembourg";
         "HU",  "Hungary";
         "MT",  "Malta";
         "NL",  "Netherlands";
         "AT",  "Austria";
         "PL",  "Poland";
         "PT",  "Portugal";
         "RO",  "Romania";
         "SI",  "Slovenia";
         "SK",  "Slovakia";
         "FI",  "Finland";
         "SE",  "Sweden";
         "UK",  "United Kingdom";
         "IS",  "Iceland";
         "NO", "Norway";
         "CH", "Switzerland";
         "ME", "Montenegro";
         "RS", "Serbia";
         "TR", "Turkey";
         "BA", "Bosnia and Herzegovina";
         "RU", "Russia";
         "US", "United States";
         "CN_X_HK", "China (except Hong Kong)";
         "JP", "Japan";
         "KR", "South Korea"] |> Map.ofList
    
  
  let sectorPerformanceUnits = 
    [
      "MIO_EUR", "Million euro";
      "EUR_HAB", "Euro per inhabitant";
      "MIO_NAC", "Million units of national currency";
      "MIO_PPS", "Million purchasing power standards (PPS)";
      "PPS_HAB_KP05", "Purchasing power standard (PPS) per inhabitant at constant 2005 prices";
      "MIO_PPS_KP05", "Million purchasing power standards (PPS) at 2005 prices";
      "PC_GDP", "Percentage of gross domestic product (GDP)";] |> Map.ofList

  let employmentIndicator = 
    [
	  "ACT", "Active population
      "AGRI_PCH	Agriculture employment growth
AGRI_RT	Agriculture in % of total employment
CH_Q_Q	Job vacancy rate quarter on quarter change
CH_Y_Y	Job vacancy rate year on year change
EM021	 Average exit age from the labour force - weighted by the probability of withdrawal from the labour market
EM022	 Average exit age from the labour force - females - weighted by the probability of withdrawal from the labour market
EM023	Average exit age from the labour force - males - weighted by the probability of withdrawal from the labour market
EMP	Total employment (domestic concept - ESA)
EMP_LFS	Total employment (resident population concept = LFS)
EMP_PCH	Employment growth
E_E	Transition employment - employment
E_I	Transition employment -inactivity
E_U	Transition employment - unemployment
INDU_RT	Industry in % of total employment
IND_PCH	Industry employment growth
I_E	Transition inactive -employment
I_I	Transition inactive - inactive
I_U	Transition inactive - unemployment
JOBOCC	Number of occupied jobs
JOBRATE	Job vacancy rate
JOBVAC	Number of job vacancies
LTU	Long-term unemployment
NSEE_AV	Persons available to work but not …
OTH	Others
SC071	 Children aged 0-17 living in jobless households: share of persons aged 0-17 who are living in households where no-one is working
SC072	People aged 18-59 living in jobless households: share of persons aged 18-59 who are living in households where no-one works
SC073	 Women aged 18-59 living in jobless households: share of women aged 18-59 who are living in households where no-one works
SC074	 Men aged 18-59 living in jobless households: share of men aged 18-59 who are living in households where no-one works
SEEK_NAV	Persons seeking work but not immediately available
SELF_EMP_RT	Self-employed in % of total employment
SERV_PCH	Services employment growth
SERV_RT	Services in % of total employment
SLACK	Labour market slack
TOTAL	Total
UEMP	Underemployed part-time workers
UNE	Unemployment (ILO)
U_E	Transition unemployment - employment
U_I	Transition unemployment - inactivity
U_U	Transition unemployment - unemployment
VLTU	 Very long-term unemployment ]  