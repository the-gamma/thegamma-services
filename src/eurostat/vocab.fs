namespace Eurostat.Vocabulary

open System
open System.IO
open FSharp.Data
open System.Collections.Generic
open Eurostat

type Country = 
  { Name : string
    Status : string 
  }

module Vocabulary =
  let getEUCountries =    
    let dict = new Dictionary<string, string>()
    dict.Add("BE", "Belgium")
    dict.Add("BG", "Bulgaria")
    dict.Add("CZ", "Czech Republic")
    dict.Add("DK", "Denmark")
    dict.Add("DE", "Germany")
    dict.Add("EE", "Estonia")
    dict.Add("IE", "Ireland")
    dict.Add("EL", "Greece")
    dict.Add("ES", "Spain")
    dict.Add("FR", "France")
    dict.Add("HR", "Croatia")
    dict.Add("IT", "Italy")
    dict.Add("CY", "Cyprus")
    dict.Add("LV", "Latvia")
    dict.Add("LU", "Luxembourg")
    dict.Add("HU",  "Hungary")
    dict.Add("MT",  "Malta")
    dict.Add("NL",  "Netherlands")
    dict.Add("AT",  "Austria")
    dict.Add("PL",  "Poland")
    dict.Add("PT",  "Portugal")
    dict.Add("RO",  "Romania")
    dict.Add("SI",  "Slovenia")
    dict.Add("SK",  "Slovakia")
    dict.Add("FI",  "Finland")
    dict.Add("SE",  "Sweden")
    dict.Add("UK",  "United Kingdom")
    IS	Iceland
	NO	Norway
	CH	Switzerland
	ME	Montenegro
	RS	Serbia
	TR	Turkey
	BA	Bosnia and Herzegovina
	RU	Russia
	US	United States
	CN_X_HK	China (except Hong Kong)
	JP	Japan
	KR	South Korea
    dict    

  let getUnit = 
  // MIO_EUR	Million euro
	// EUR_HAB	Euro per inhabitant
	// MIO_NAC	Million units of national currency
	// MIO_PPS	Million purchasing power standards (PPS)
	// PPS_HAB_KP05	Purchasing power standard (PPS) per inhabitant at constant 2005 prices
	// MIO_PPS_KP05	Million purchasing power standards (PPS) at 2005 prices
	// PC_GDP	Percentage of gross domestic product (GDP)

  let getSecPerf = 
  // MIO_EUR	Million euro
	// EUR_HAB	Euro per inhabitant
	// MIO_NAC	Million units of national currency
	// MIO_PPS	Million purchasing power standards (PPS)
	// PPS_HAB_KP05	Purchasing power standard (PPS) per inhabitant at constant 2005 prices
	// MIO_PPS_KP05	Million purchasing power standards (PPS) at 2005 prices
	// PC_GDP	Percentage of gross domestic product (GDP)

  EU28	European Union (28 countries)
	EA19	Euro area (19 countries)
	BE	Belgium
	BG	Bulgaria
	CZ	Czech Republic
	DK	Denmark
	DE	Germany (until 1990 former territory of the FRG)
	EE	Estonia
	IE	Ireland
	EL	Greece
	ES	Spain
	FR	France
	HR	Croatia
	IT	Italy
	CY	Cyprus
	LV	Latvia
	LT	Lithuania
	LU	Luxembourg
	HU	Hungary
	MT	Malta
	NL	Netherlands
	AT	Austria
	PL	Poland
	PT	Portugal
	RO	Romania
	SI	Slovenia
	SK	Slovakia
	FI	Finland
	SE	Sweden
	UK	United Kingdom
	IS	Iceland
	NO	Norway
	CH	Switzerland
	ME	Montenegro
	RS	Serbia
	TR	Turkey
	BA	Bosnia and Herzegovina
	RU	Russia
	US	United States
	CN_X_HK	China (except Hong Kong)
	JP	Japan
	KR	South Korea