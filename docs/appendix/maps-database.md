
## mapdata.airport

|   Column   |         Type          | 
|------------|-----------------------|
| arpt_id    | character varying(4)  |  
| name       | character varying(42) |  
| city       | character varying(40) |  
| state      | character varying(2)  |  
| siteno     | character varying(9)  |  
| site_type  | character varying(1)  |  
| fac_use    | character varying(2)  |  
| owner_type | character varying(2)  |  
| elv        | integer               |  
| latitude   | character varying(16) |  
| longitude  | character varying(16) |  
| lon        | double precision      |  
| lat        | double precision      |  
| the_geom   | geometry(Point,4326)  |  

ok

## mapdata.allrivers

|     Column     |              Type              |
|----------------|--------------------------------|
| ihabbsrf_i     | double precision               |    
| rr             | character varying(11)          |    
| huc            | integer                        |    
| type           | character varying(1)           |    
| pmile          | double precision               |    
| pname          | character varying(30)          |    
| owname         | character varying(30)          |    
| pnmcd          | character varying(11)          |    
| ownmcd         | character varying(11)          |    
| dsrr           | double precision               |    
| dshuc          | integer                        |    
| usdir          | character varying(1)           |    
| lev            | smallint                       |    
| j              | smallint                       |    
| termid         | integer                        |    
| trmblv         | smallint                       |    
| k              | smallint                       |    
| the_geom       | geometry(MultiLineString,4326) |    



## mapdata.artcc
|   Column   |            Type             | 
|------------|-----------------------------|
| artcc      | character varying(4)        |
| alt        | character varying(1)        |
| name       | character varying(30)       |
| type       | character varying(5)        |
| city       | character varying(40)       |
| id         | double precision            |
| the_geom   | geometry(MultiPolygon,4326) |




## mapdata.basins
|     Column     |            Type             |
|----------------|-----------------------------|
| rfc            | character varying(7)        | 
| cwa            | character varying(5)        | 
| id             | character varying(8)        | 
| name           | character varying(64)       | 
| lon            | double precision            | 
| lat            | double precision            | 
| the_geom       | geometry(MultiPolygon,4326) | 
| the_geom_0     | geometry(MultiPolygon,4326) | 
| the_geom_0_064 | geometry(MultiPolygon,4326) | 
| the_geom_0_016 | geometry(MultiPolygon,4326) | 
| the_geom_0_004 | geometry(MultiPolygon,4326) | 
| the_geom_0_001 | geometry(MultiPolygon,4326) | 




## mapdata.canada
|     Column     |            Type             |
|----------------|-----------------------------|
| f_code         | character varying(5)        | 
| name_en        | character varying(25)       | 
| nom_fr         | character varying(25)       | 
| country        | character varying(3)        | 
| cgns_fid       | character varying(32)       | 
| the_geom       | geometry(MultiPolygon,4326) | 




## mapdata.city
|   Column   |          Type          |
|------------|------------------------|
| st_fips    | character varying(4)   |
| sfips      | character varying(2)   |
| county_fip | character varying(4)   |
| cfips      | character varying(4)   |
| pl_fips    | character varying(7)   |
| id         | character varying(20)  |
| name       | character varying(39)  |
| elevation  | character varying(60)  |
| pop_1990   | numeric                |
| population | character varying(30)  |
| st         | character varying(6)   |
| warngenlev | character varying(16)  |
| warngentyp | character varying(16)  |
| watch_warn | character varying(3)   |
| zwatch_war | double precision       | 
| prog_disc  | integer                | 
| zprog_disc | double precision       | 
| comboflag  | double precision       | 
| land_water | character varying(16)  |
| recnum     | double precision       | 
| lon        | double precision       | 
| lat        | double precision       | 
| f3         | double precision       | 
| f4         | character varying(254) |
| f6         | double precision       | 
| state      | character varying(25)  |
| the_geom   | geometry(Point,4326)   |

## mapdata.county
|     Column     |            Type             |
|----------------|-----------------------------|
| state          | character varying(2)        | 
| cwa            | character varying(9)        | 
| countyname     | character varying(24)       | 
| fips           | character varying(5)        | 
| time_zone      | character varying(2)        | 
| fe_area        | character varying(2)        | 
| lon            | numeric                     | 
| lat            | numeric                     | 
| the_geom       | geometry(MultiPolygon,4326) | 

## mapdata.customlocations
|   Column   |            Type             |
|------------|-----------------------------|
| bullet     | character varying(16)       |     
| name       | character varying(64)       |     
| cwa        | character varying(12)       |     
| rfc        | character varying(8)        |     
| lon        | numeric                     |     
| lat        | numeric                     |     
| the_geom   | geometry(MultiPolygon,4326) |     

## mapdata.cwa
|     Column     |            Type             |
|----------------|-----------------------------|
| cwa            | character varying(9)        |
| wfo            | character varying(3)        |
| lon            | numeric                     |
| lat            | numeric                     |
| region         | character varying(2)        |
| fullstaid      | character varying(4)        |
| citystate      | character varying(50)       |
| city           | character varying(50)       |
| state          | character varying(50)       |
| st             | character varying(2)        |
| the_geom       | geometry(MultiPolygon,4326) |

## mapdata.firewxaor
|     Column     |            Type             |
|----------------|-----------------------------|
| cwa            | character varying(3)        |    
| wfo            | character varying(3)        |    
| the_geom       | geometry(MultiPolygon,4326) |    

## mapdata.firewxzones
|     Column     |            Type             | 
|----------------|-----------------------------|
| state          | character varying(2)        | 
| zone           | character varying(3)        | 
| cwa            | character varying(3)        | 
| name           | character varying(254)      | 
| state_zone     | character varying(5)        | 
| time_zone      | character varying(2)        | 
| fe_area        | character varying(2)        | 
| lon            | numeric                     | 
| lat            | numeric                     | 
| the_geom       | geometry(MultiPolygon,4326) | 

## mapdata.fix
|   Column   |         Type          |
|------------|-----------------------|
| id         | character varying(30) |
| type       | character varying(2)  |
| use        | character varying(5)  |
| state      | character varying(2)  |
| min_alt    | integer               |
| latitude   | character varying(16) |
| longitude  | character varying(16) |
| lon        | double precision      |
| lat        | double precision      |
| the_geom   | geometry(Point,4326)  |

## mapdata.highaltitude
|   Column   |              Type              |
|------------|--------------------------------|
| awy_des    | character varying(2)           |  
| awy_id     | character varying(12)          |  
| awy_type   | character varying(1)           |  
| airway     | character varying(16)          |  
| newfield1  | double precision               |  
| the_geom   | geometry(MultiLineString,4326) |  

## mapdata.highsea
|     Column     |            Type             | 
|----------------|-----------------------------|
| wfo            | character varying(3)        |  
| name           | character varying(250)      |  
| lat            | numeric                     |  
| lon            | numeric                     |  
| id             | character varying(5)        |  
| the_geom       | geometry(MultiPolygon,4326) |  

## mapdata.highway
|     Column     |              Type              | 
|----------------|--------------------------------|
| prefix         | character varying(2)           |  
| pretype        | character varying(6)           |  
| name           | character varying(30)          |  
| type           | character varying(6)           |  
| suffix         | character varying(2)           |  
| class          | character varying(1)           |  
| class_rte      | character varying(1)           |  
| hwy_type       | character varying(1)           |  
| hwy_symbol     | character varying(20)          |  
| route          | character varying(25)          |  
| the_geom       | geometry(MultiLineString,4326) |  

## mapdata.hsa
|     Column     |            Type             |
|----------------|-----------------------------|
| wfo            | character varying(3)        |
| lon            | double precision            |
| lat            | double precision            |
| the_geom       | geometry(MultiPolygon,4326) |

## mapdata.interstate
|     Column     |              Type              |
|----------------|--------------------------------|
| prefix         | character varying(2)           |
| pretype        |Ushy<br>Hwy<br>Ave<br>Cord<br>Rt<br>Loop<br>I<br>Sthy|
| name           | character varying(30)          |
| type           | character varying(6)           |
| suffix         | character varying(2)           |
| hwy_type       |I<br>U<br>S           |
| hwy_symbol     | character varying(20)          |
| route          | character varying(25)          |
| the_geom       | geometry(MultiLineString,4326) |

## mapdata.isc
|   Column   |            Type             |
|------------|-----------------------------|
| wfo        | character varying(3)        |
| cwa        | character varying(3)        |
| the_geom   | geometry(MultiPolygon,4326) |

## mapdata.lake
|     Column     |            Type             |
|----------------|-----------------------------|
| name           | character varying(40)       |
| feature        | character varying(40)       |
| lon            | double precision            | 
| lat            | double precision            | 
| the_geom       | geometry(MultiPolygon,4326) |

## mapdata.latlon10
|   Column   |              Type              |
|------------|--------------------------------|
| the_geom   | geometry(MultiLineString,4326) |

## mapdata.lowaltitude
|   Column   |              Type              | 
|------------|--------------------------------|
| awy_des    | character varying(2)           | 
| awy_id     | character varying(12)          | 
| awy_type   | character varying(1)           | 
| airway     | character varying(16)          | 
| newfield1  | double precision               | 
| the_geom   | geometry(MultiLineString,4326) | 

## mapdata.majorrivers
|     Column     |              Type              | 
|----------------|--------------------------------|
| rf1_150_id     | double precision               | 
| huc            | integer                        | 
| seg            | smallint                       | 
| milept         | double precision               | 
| seqno          | double precision               | 
| rflag          | character varying(1)           | 
| owflag         | character varying(1)           | 
| tflag          | character varying(1)           | 
| sflag          | character varying(1)           | 
| type           | character varying(1)           | 
| segl           | double precision               | 
| lev            | smallint                       | 
| j              | smallint                       | 
| k              | smallint                       | 
| pmile          | double precision               | 
| arbsum         | double precision               | 
| usdir          | character varying(1)           | 
| termid         | integer                        | 
| trmblv         | smallint                       | 
| pname          | character varying(30)          | 
| pnmcd          | character varying(11)          | 
| owname         | character varying(30)          | 
| ownmcd         | character varying(11)          | 
| dshuc          | integer                        | 
| dsseg          | smallint                       | 
| dsmlpt         | double precision               | 
| editrf1_       | double precision               | 
| demand         | double precision               | 
| ftimped        | double precision               | 
| tfimped        | double precision               | 
| dir            | double precision               | 
| rescode        | double precision               | 
| center         | double precision               | 
| erf1__         | double precision               | 
| reservoir_     | double precision               | 
| pname_res      | character varying(30)          | 
| pnmcd_res      | character varying(11)          | 
| meanq          | double precision               | 
| lowq           | double precision               | 
| meanv          | double precision               | 
| lowv           | double precision               | 
| worka          | double precision               | 
| gagecode       | double precision               | 
| strahler       | double precision               | 
| rr             | character varying(11)          | 
| dsrr           | double precision               | 
| huc2           | smallint                       | 
| huc4           | smallint                       | 
| huc6           | integer                        | 
| the_geom       | geometry(MultiLineString,4326) | 

## mapdata.marinesites
|   Column   |         Type          | 
|------------|-----------------------|
| st         | character varying(3)  | 
| name       | character varying(50) | 
| prog_disc  | bigint                | 
| warngenlev | character varying(14) | 
| the_geom   | geometry(Point,4326)  | 

## mapdata.marinezones
|     Column     |            Type             | 
|----------------|-----------------------------|
| id             | character varying(6)        | 
| wfo            | character varying(3)        | 
| gl_wfo         | character varying(3)        | 
| name           | character varying(254)      | 
| ajoin0         | character varying(6)        | 
| ajoin1         | character varying(6)        | 
| lon            | numeric                     | 
| lat            | numeric                     | 
| the_geom       | geometry(MultiPolygon,4326) | 

## mapdata.mexico
|     Column     |            Type             |
|----------------|-----------------------------|
| area           | double precision            | 
| perimeter      | double precision            | 
| st_mx_         | double precision            | 
| st_mx_id       | double precision            | 
| name           | character varying(66)       | 
| country        | character varying(127)      | 
| continent      | character varying(127)      | 
| the_geom       | geometry(MultiPolygon,4326) | 

## mapdata.navaid
|   Column   |          Type          |
|------------|------------------------|
| id         | character varying(30)  | 
| clscode    | character varying(11)  | 
| city       | character varying(40)  | 
| elv        | integer                | 
| freq       | double precision       | 
| name       | character varying(30)  | 
| status     | character varying(30)  | 
| type       | character varying(25)  | 
| oprhours   | character varying(11)  | 
| oprname    | character varying(50)  | 
| latdms     | character varying(16)  | 
| londms     | character varying(16)  | 
| airway     | character varying(254) | 
| sym        | smallint               | 
| lon        | double precision       | 
| lat        | double precision       | 
| the_geom   | geometry(Point,4326)   | 

## mapdata.offshore
|   Column   |            Type             |
|------------|-----------------------------|
| id         | character varying(50)       |   
| wfo        | character varying(10)       |   
| lon        | numeric                     |   
| lat        | numeric                     |   
| location   | character varying(70)       |   
| name       | character varying(90)       |   
| the_geom   | geometry(MultiPolygon,4326) |   

## mapdata.railroad
|     Column     |              Type              |
|----------------|--------------------------------|
| fnode_         | double precision               |   
| tnode_         | double precision               |   
| lpoly_         | double precision               |   
| rpoly_         | double precision               |   
| length         | numeric                        |   
| railrdl021     | double precision               |   
| railrdl020     | double precision               |   
| feature        | character varying(18)          |   
| name           | character varying(43)          |   
| state          | character varying(2)           |   
| state_fips     | character varying(2)           |   
| the_geom       | geometry(MultiLineString,4326) |   

## mapdata.rfc
|     Column     |            Type             |
|----------------|-----------------------------|
| site_id        | character varying(3)        |
| state          | character varying(2)        |
| rfc_name       | character varying(18)       |
| rfc_city       | character varying(25)       |
| basin_id       | character varying(5)        |
| the_geom       | geometry(MultiPolygon,4326) |

## mapdata.specialuse
|   Column   |            Type             |
|------------|-----------------------------|
| name       | character varying(32)       |
| code       | character varying(16)       |
| yn         | smallint                    |
| alt_desc   | character varying(128)      |
| artcc      | character varying(4)        |
| ctr_agen   | character varying(128)      |
| sch_agen   | character varying(128)      |
| state      | character varying(2)        |
| the_geom   | geometry(MultiPolygon,4326) |



## mapdata.states
|     Column     |            Type             |
|----------------|-----------------------------|
| state          | character varying(2)        | 
| name           | character varying(24)       | 
| fips           | character varying(2)        | 
| lon            | numeric                     | 
| lat            | numeric                     | 
| the_geom       | geometry(MultiPolygon,4326) | 



## mapdata.timezones
|   Column   |            Type             |
|------------|-----------------------------|
| name       | character varying(50)       |    
| time_zone  | character varying(1)        |    
| standard   | character varying(9)        |    
| advanced   | character varying(10)       |    
| unix_time  | character varying(19)       |    
| lon        | double precision            |    
| lat        | double precision            |    
| the_geom   | geometry(MultiPolygon,4326) |    


## mapdata.warngenloc
|   Column   |            Type             |
|------------|-----------------------------|
| name       | character varying(254)      |
| st         | character varying(3)        |
| state      | character varying(20)       |
| population | integer                     |
| warngenlev | integer                     |
| cwa        | character varying(4)        |
| goodness   | double precision            |
| lat        | numeric                     |
| lon        | numeric                     |
| usedirs    | numeric(10,0)               |
| supdirs    | character varying(20)       |
| landwater  | character varying(3)        |
| recnum     | integer                     |
| the_geom   | geometry(MultiPolygon,4326) |



## mapdata.world
|   Column   |            Type             | 
|------------|-----------------------------|
| name       | character varying(30)       |
| count      | double precision            |
| first_coun | character varying(2)        |
| first_regi | character varying(1)        |
| the_geom   | geometry(MultiPolygon,4326) |



## mapdata.zone
|     Column     |            Type             |
|----------------|-----------------------------|
| state          | character varying(2)        |
| cwa            | character varying(9)        |
| time_zone      | character varying(2)        |
| fe_area        | character varying(2)        |
| zone           | character varying(3)        |
| name           | character varying(254)      |
| state_zone     | character varying(5)        |
| lon            | numeric                     |
| lat            | numeric                     |
| shortname      | character varying(32)       |
| the_geom       | geometry(MultiPolygon,4326) |
