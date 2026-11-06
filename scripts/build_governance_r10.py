import csv, io, unicodedata, os
BASE='/sessions/happy-peaceful-goldberg/gov/governance2019-master/data'

R10 = {
"Africa (R10)": ("""Algeria Angola Benin Botswana BurkinaFaso Burundi Cameroon CaboVerde CentralAfricanRepublic Chad Comoros Congo CotedIvoire DemocraticRepublicoftheCongo Djibouti Egypt EquatorialGuinea Eritrea Ethiopia Gabon Gambia Ghana Guinea GuineaBissau Kenya Lesotho Liberia Libya Madagascar Malawi Mali Mauritania Mauritius Mayotte Morocco Mozambique Namibia Niger Nigeria Rwanda Reunion SaoTomeandPrincipe Senegal Seychelles SierraLeone Somalia SouthAfrica SouthSudan Sudan Eswatini Togo Tunisia Uganda Tanzania WesternSahara Zambia Zimbabwe""",
"""DZA AGO BEN BWA BFA BDI CMR CPV CAF TCD COM COG CIV COD DJI EGY GNQ ERI ETH GAB GMB GHA GIN GNB KEN LSO LBR LBY MDG MWI MLI MRT MUS MYT MAR MOZ NAM NER NGA RWA REU STP SEN SYC SLE SOM ZAF SSD SDN SWZ TGO TUN UGA TZA ESH ZMB ZWE"""),
"China+ (R10)": ("China HongKong Macao Mongolia NorthKorea SouthKorea", "CHN HKG MAC MNG PRK KOR"),
"Europe (R10)": ("""Albania Austria Belgium BosniaandHerzegovina Bulgaria Croatia Cyprus Czechia Denmark Estonia France Finland Germany Greece Hungary Iceland Ireland Italy Kosovo Latvia Lithuania Luxembourg Malta Montenegro Netherlands NorthMacedonia Norway Portugal Switzerland Poland Romania Serbia Slovakia Slovenia Spain Sweden Turkey UnitedKingdom""",
"""ALB AUT BEL BIH BGR HRV CYP CZE DNK EST FRA FIN DEU GRC HUN ISL IRL ITA XKX LVA LTU LUX MLT MNE NLD MKD NOR PRT CHE POL ROU SRB SVK SVN ESP SWE TUR GBR"""),
"India+ (R10)": ("India Afghanistan Bangladesh Bhutan Maldives Nepal Pakistan SriLanka", "IND AFG BGD BTN MDV NPL PAK LKA"),
"Latin America (R10)": ("""AntiguaandBarbuda Argentina Aruba Bahamas Barbados Belize Bolivia Brazil Chile Colombia CostaRica Cuba Curacao DominicanRepublic Ecuador ElSalvador FrenchGuiana Grenada Guadeloupe Guatemala Guyana Haiti Honduras Jamaica Martinique Mexico Nicaragua Panama Paraguay Peru PuertoRico SaintKittsandNevis SaintLucia SaintVincentandtheGrenadines SintMaarten Suriname TrinidadandTobago UnitedStatesVirginIslands Uruguay Venezuela""",
"""ATG ARG ABW BHS BRB BLZ BOL BRA CHL COL CRI CUB CUW DOM ECU SLV GUF GRD GLP GTM GUY HTI HND JAM MTQ MEX NIC PAN PRY PER PRI KNA LCA VCT SXM SUR TTO VIR URY VEN"""),
"Middle East (R10)": ("Bahrain Iraq Iran Israel Jordan Kuwait Lebanon Oman Palestine Qatar SaudiArabia Syria UnitedArabEmirates Yemen",
"BHR IRQ IRN ISR JOR KWT LBN OMN PSE QAT SAU SYR ARE YEM"),
"North America (R10)": ("Canada Guam UnitedStates", "CAN GUM USA"),
"Pacific OECD (R10)": ("Australia Japan NewZealand", "AUS JPN NZL"),
"Reforming Economies (R10)": ("Armenia Azerbaijan Belarus Georgia Kazakhstan Kyrgyzstan Moldova RussianFederation Tajikistan Turkmenistan Ukraine Uzbekistan",
"ARM AZE BLR GEO KAZ KGZ MDA RUS TJK TKM UKR UZB"),
"Rest of Asia (R10)": ("""BruneiDarussalam Cambodia CookIslands Fiji FrenchPolynesia Indonesia Kiribati Laos Malaysia MarshallIslands Micronesia Myanmar Nauru Niue Palau NewCaledonia PapuaNewGuinea Philippines Samoa Singapore SolomonIslands Taiwan Thailand TimorLeste Tonga Tuvalu Vanuatu VietNam""",
"""BRN KHM COK FJI PYF IDN KIR LAO MYS MHL FSM MMR NRU NIU PLW NCL PNG PHL WSM SGP SLB TWN THA TLS TON TUV VUT VNM"""),
}

iso2r10={}; name2iso={}
for reg,(names,isos) in R10.items():
    n=names.split(); i=isos.split()
    assert len(n)==len(i), (reg, len(n), len(i))
    for a,b in zip(n,i):
        iso2r10[b]=reg; name2iso[a]=b
print("regions ok, countries:", len(iso2r10))

def norm(s):
    s=unicodedata.normalize('NFKD',s).encode('ascii','ignore').decode()
    return ''.join(ch for ch in s if ch.isalnum()).lower()

name2iso_n={norm(k):v for k,v in name2iso.items()}
ALIAS={'unitedkingdomofgreatbritainandnorthernireland':'GBR',
 'theformeryugoslavrepublicofmacedonia':'MKD',
 'boliviaplurinationalstateof':'BOL',
 'libyanarabjamahiriya':'LBY','occupiedpalestinianterritory':'PSE',
 'hongkongspecialadministrativeregionofchina':'HKG',
 'macaospecialadministrativeregionofchina':'MAC',
 'russia':'RUS','vietnam':'VNM','southkorea':'KOR','republicofkorea':'KOR',
 'korea':'KOR','northkorea':'PRK','democraticpeoplesrepublicofkorea':'PRK',
 'iranislamicrepublicof':'IRN','iran':'IRN','syrianarabrepublic':'SYR','syria':'SYR',
 'laopeoplesdemocraticrepublic':'LAO','laos':'LAO','bolivia':'BOL',
 'venezuelabolivarianrepublicof':'VEN','venezuela':'VEN','tanzania':'TZA',
 'unitedrepublicoftanzania':'TZA','moldova':'MDA','republicofmoldova':'MDA',
 'macedonia':'MKD','tfyrmacedonia':'MKD','northmacedonia':'MKD','czechrepublic':'CZE',
 'czechia':'CZE','swaziland':'SWZ','eswatini':'SWZ','capeverde':'CPV','caboverde':'CPV',
 'cotedivoire':'CIV','ivorycoast':'CIV','drcongo':'COD','congodemocraticrepublicofthe':'COD',
 'democraticrepublicofthecongo':'COD','congo':'COG','republicofcongo':'COG',
 'unitedstatesofamerica':'USA','unitedstates':'USA','unitedkingdom':'GBR',
 'palestine':'PSE','stateofpalestine':'PSE','brunei':'BRN','bruneidarussalam':'BRN',
 'timorleste':'TLS','easttimor':'TLS','micronesiafederatedstatesof':'FSM',
 'chinahongkongsar':'HKG','chinamacaosar':'MAC','hongkong':'HKG','macao':'MAC',
 'taiwan':'TWN','chinataiwanprovinceof':'TWN','curacao':'CUW','gambia':'GMB',
 'thegambia':'GMB','reunion':'REU','sintmaartendutchpart':'SXM',
 'unitedstatesvirginislands':'VIR','virginislandsus':'VIR'}

# population (SSP2)
txt=open(os.path.join(BASE,'pop_size.csv'),encoding='utf-8',errors='replace').read()
rows=list(csv.DictReader(io.StringIO(txt[txt.index('"Area","Scenario","ISOCode"'):])))
pop={}; unmatched=set()
for r in rows:
    if r['Scenario']!='SSP2': continue
    k=norm(r['Area'])
    iso=name2iso_n.get(k) or ALIAS.get(k)
    if not iso: unmatched.add(r['Area']); continue
    pop[(iso,int(r['Year']))]=float(r['Population'])
print("unmatched population areas:", sorted(unmatched))

# governance (SSP2)
gov={}
for r in csv.DictReader(open(os.path.join(BASE,'governance_obs_project.csv'))):
    if r['scenario']!='SSP2': continue
    gov[(r['countrycode'],int(r['year']))]=float(r['governance'])
gov_isos=set(c for c,_ in gov)
print("governance countries not in R10 map:", sorted(gov_isos-set(iso2r10)))

# aggregate
years=list(range(2020,2101,10))
out=[]
for y in years:
    for reg in R10:
        num=den=0.0; n=0
        for iso,rr in iso2r10.items():
            if rr!=reg: continue
            g=gov.get((iso,min(y,2099)))
            # population on 5-year grid
            p=pop.get((iso,y)) or pop.get((iso,y-(y%5)))
            if g is None or p is None: continue
            num+=g*p; den+=p; n+=1
        out.append((reg,y,round(num/den,4) if den else '', n))
os.makedirs('/sessions/happy-peaceful-goldberg/mnt/prisma_wp6_analysis/data',exist_ok=True)
with open('/sessions/happy-peaceful-goldberg/mnt/prisma_wp6_analysis/data/governance_r10_ssp2.csv','w',newline='') as f:
    w=csv.writer(f); w.writerow(['region','year','governance','n_countries']); w.writerows(out)
with open('/sessions/happy-peaceful-goldberg/mnt/prisma_wp6_analysis/data/country_to_r10.csv','w',newline='') as f:
    w=csv.writer(f); w.writerow(['iso3','r10_region'])
    for iso,reg in sorted(iso2r10.items()): w.writerow([iso,reg])
print()
print(f"{'region':28}" + ''.join(f'{y:>7}' for y in years))
for reg in R10:
    vals=[v for r,y,v,n in out if r==reg]
    print(f'{reg:28}' + ''.join(f'{v:7.2f}' if v!='' else '    n/a' for v in vals))
