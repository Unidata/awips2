/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- 
-- $Id: lwpostgis.sql.in 2756 2008-04-07 19:47:05Z pramsey $
--
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.refractions.net
-- Copyright 2001-2003 Refractions Research Inc.
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--
-- WARNING: Any change in this file must be evaluated for compatibility.
--          Changes cleanly handled by lwpostgis_uptrade.sql are fine,
--	    other changes will require a bump in Major version.
--	    Currently only function replaceble by CREATE OR REPLACE
--	    are cleanly handled.
--
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -














BEGIN;

-------------------------------------------------------------------
--  HISTOGRAM2D TYPE (lwhistogram2d)
-------------------------------------------------------------------









-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION histogram2d_in(cstring)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwhistogram2d_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_histogram2d_in(cstring)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwhistogram2d_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION histogram2d_out(histogram2d)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwhistogram2d_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_histogram2d_out(histogram2d)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwhistogram2d_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE TYPE histogram2d (
	alignment = double,
	internallength = variable,
	input = ST_histogram2d_in,
	output = ST_histogram2d_out,
	storage = main
);

-------------------------------------------------------------------
--  SPHEROID TYPE
-------------------------------------------------------------------









-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION spheroid_in(cstring)
	RETURNS spheroid
	AS '%{INSTALL_PATH}%/lib/liblwgeom','ellipsoid_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_spheroid_in(cstring)
	RETURNS spheroid
	AS '%{INSTALL_PATH}%/lib/liblwgeom','ellipsoid_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION spheroid_out(spheroid)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom','ellipsoid_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_spheroid_out(spheroid)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom','ellipsoid_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE TYPE spheroid (
	alignment = double,
	internallength = 65,
	input = ST_spheroid_in,
	output = ST_spheroid_out
);

-------------------------------------------------------------------
--  GEOMETRY TYPE (lwgeom)
-------------------------------------------------------------------









-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_in(cstring)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_in'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_in(cstring)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_in'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_out(geometry)
        RETURNS cstring
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_out'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_out(geometry)
        RETURNS cstring
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_out'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_analyze(internal)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_analyze'
	LANGUAGE 'C' VOLATILE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_analyze(internal)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_analyze'
	LANGUAGE 'C' VOLATILE STRICT; -- WITH (isstrict);



-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_recv(internal)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_recv'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_recv(internal)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_recv'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_send(geometry)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_send'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_send(geometry)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_send'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);


CREATE TYPE geometry (
        internallength = variable,
        input = ST_geometry_in,
        output = ST_geometry_out,

	send = ST_geometry_send,
	receive = ST_geometry_recv,

	delimiter = ':',

	analyze = ST_geometry_analyze,

        storage = main
);

-------------------------------------------
-- Affine transforms
-------------------------------------------

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Affine(geometry,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_affine'
	LANGUAGE 'C' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Affine(geometry,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8,float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_affine'
	LANGUAGE 'C' IMMUTABLE STRICT; 

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Affine(geometry,float8,float8,float8,float8,float8,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  $2, $3, 0,  $4, $5, 0,  0, 0, 1,  $6, $7, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Affine(geometry,float8,float8,float8,float8,float8,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  $2, $3, 0,  $4, $5, 0,  0, 0, 1,  $6, $7, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION RotateZ(geometry,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  cos($2), -sin($2), 0,  sin($2), cos($2), 0,  0, 0, 1,  0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_RotateZ(geometry,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  cos($2), -sin($2), 0,  sin($2), cos($2), 0,  0, 0, 1,  0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Rotate(geometry,float8)
	RETURNS geometry
	AS 'SELECT rotateZ($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Rotate(geometry,float8)
	RETURNS geometry
	AS 'SELECT rotateZ($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION RotateX(geometry,float8)
	RETURNS geometry
 	AS 'SELECT affine($1, 1, 0, 0, 0, cos($2), -sin($2), 0, sin($2), cos($2), 0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_RotateX(geometry,float8)
	RETURNS geometry
 	AS 'SELECT affine($1, 1, 0, 0, 0, cos($2), -sin($2), 0, sin($2), cos($2), 0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION RotateY(geometry,float8)
	RETURNS geometry
 	AS 'SELECT affine($1,  cos($2), 0, sin($2),  0, 1, 0,  -sin($2), 0, cos($2), 0,  0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_RotateY(geometry,float8)
	RETURNS geometry
 	AS 'SELECT affine($1,  cos($2), 0, sin($2),  0, 1, 0,  -sin($2), 0, cos($2), 0,  0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Translate(geometry,float8,float8,float8)
	RETURNS geometry
 	AS 'SELECT affine($1, 1, 0, 0, 0, 1, 0, 0, 0, 1, $2, $3, $4)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Translate(geometry,float8,float8,float8)
	RETURNS geometry
 	AS 'SELECT affine($1, 1, 0, 0, 0, 1, 0, 0, 0, 1, $2, $3, $4)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Translate(geometry,float8,float8)
	RETURNS geometry
	AS 'SELECT translate($1, $2, $3, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Translate(geometry,float8,float8)
	RETURNS geometry
	AS 'SELECT translate($1, $2, $3, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT;

-- Availability: 1.1.0
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Scale(geometry,float8,float8,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  $2, 0, 0,  0, $3, 0,  0, 0, $4,  0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Scale(geometry,float8,float8,float8)
	RETURNS geometry
	AS 'SELECT affine($1,  $2, 0, 0,  0, $3, 0,  0, 0, $4,  0, 0, 0)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.0
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Scale(geometry,float8,float8)
	RETURNS geometry
	AS 'SELECT scale($1, $2, $3, 1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Scale(geometry,float8,float8)
	RETURNS geometry
	AS 'SELECT scale($1, $2, $3, 1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; 

-- Availability: 1.1.0 
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION transscale(geometry,float8,float8,float8,float8)
        RETURNS geometry
        AS 'SELECT affine($1,  $4, 0, 0,  0, $5, 0, 
		0, 0, 1,  $2 * $4, $3 * $5, 0)'
        LANGUAGE 'SQL' IMMUTABLE STRICT;

-- Availability: 1.2.2 
CREATE OR REPLACE FUNCTION ST_transscale(geometry,float8,float8,float8,float8)
        RETURNS geometry
        AS 'SELECT affine($1,  $4, 0, 0,  0, $5, 0, 
		0, 0, 1,  $2 * $4, $3 * $5, 0)'
        LANGUAGE 'SQL' IMMUTABLE STRICT;

-- Availability: 1.1.0
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION shift_longitude(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_longitude_shift'
	LANGUAGE 'C' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_shift_longitude(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_longitude_shift'
	LANGUAGE 'C' IMMUTABLE STRICT; 

-------------------------------------------------------------------
--  BOX3D TYPE
-------------------------------------------------------------------









-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box3d_in(cstring)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box3d_out(box3d)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box3d_in(cstring)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box3d_out(box3d)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE TYPE box3d (
	alignment = double,
	internallength = 48,
	input = ST_box3d_in,
	output = ST_box3d_out
);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION xmin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_xmin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_XMin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_xmin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ymin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_ymin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_YMin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_ymin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION zmin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_zmin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_ZMin(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_zmin'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION xmax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_xmax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_XMax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_xmax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ymax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_ymax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_YMax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_ymax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION zmax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_zmax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_ZMax(box3d)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_zmax'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-------------------------------------------------------------------
--  CHIP TYPE
-------------------------------------------------------------------









-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION chip_in(cstring)
	RETURNS chip
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);
        
-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_chip_in(cstring)
	RETURNS chip
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_in'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION chip_out(chip)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_chip_out(chip)
	RETURNS cstring
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_out'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE TYPE chip (
	alignment = double,
	internallength = variable,
	input = ST_chip_in,
	output = ST_chip_out,
	storage = extended
);

-----------------------------------------------------------------------
-- BOX2D
-----------------------------------------------------------------------










-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_in(cstring)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_in'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_in(cstring)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_in'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_out(box2d)
        RETURNS cstring
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_out'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_out(box2d)
        RETURNS cstring
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_out'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

CREATE TYPE box2d (
        internallength = 16,
        input = ST_box2d_in,
        output = ST_box2d_out,
        storage = plain
);

---- BOX2D  support functions

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_overleft(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overleft'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_overleft(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overleft'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_overright(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overright' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_overright(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overright' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_left(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_left' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_left(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_left' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_right(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_right' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_right(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_right' 
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_contain(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_contain'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_contain(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_contain'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_contained(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_contained'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_contained(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_contained'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_overlap(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overlap'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_overlap(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_overlap'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_same(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_same'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_same(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_same'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d_intersects(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_intersects'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d_intersects(box2d, box2d) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2D_intersects'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-- lwgeom  operator support functions

-------------------------------------------------------------------
-- BTREE indexes
-------------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_lt(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_lt'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_lt(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_lt'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_le(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_le'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_le(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_le'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_gt(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_gt'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_gt(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_gt'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_ge(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_ge'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_ge(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_ge'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_eq(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_eq'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_eq(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_eq'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_cmp(geometry, geometry) 
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_cmp'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_cmp(geometry, geometry) 
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'lwgeom_cmp'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

--
-- Sorting operators for Btree
--

CREATE OPERATOR < (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_lt,
   COMMUTATOR = '>', NEGATOR = '>=',
   RESTRICT = contsel, JOIN = contjoinsel
);

CREATE OPERATOR <= (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_le,
   COMMUTATOR = '>=', NEGATOR = '>',
   RESTRICT = contsel, JOIN = contjoinsel
);

CREATE OPERATOR = (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_eq,
   COMMUTATOR = '=', -- we might implement a faster negator here
   RESTRICT = contsel, JOIN = contjoinsel
);

CREATE OPERATOR >= (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_ge,
   COMMUTATOR = '<=', NEGATOR = '<',
   RESTRICT = contsel, JOIN = contjoinsel
);
CREATE OPERATOR > (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_gt,
   COMMUTATOR = '<', NEGATOR = '<=',
   RESTRICT = contsel, JOIN = contjoinsel
);



CREATE OPERATOR CLASS btree_geometry_ops
	DEFAULT FOR TYPE geometry USING btree AS
	OPERATOR	1	< ,
	OPERATOR	2	<= ,
	OPERATOR	3	= ,
	OPERATOR	4	>= ,
	OPERATOR	5	> ,
	FUNCTION	1	geometry_cmp (geometry, geometry);




-------------------------------------------------------------------
-- GiST indexes
-------------------------------------------------------------------
-- Deprecation in 1.2.3



CREATE OR REPLACE FUNCTION postgis_gist_sel (internal, oid, internal, int4)

	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_gist_sel'
	LANGUAGE 'C';

-- Availability: 1.2.2



CREATE OR REPLACE FUNCTION ST_postgis_gist_sel (internal, oid, internal, int4)

	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_gist_sel'
	LANGUAGE 'C';

-- Deprecation in 1.2.3





CREATE OR REPLACE FUNCTION postgis_gist_joinsel(internal, oid, internal, smallint)

	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_gist_joinsel'
	LANGUAGE 'C';

-- Availability: 1.2.2





CREATE OR REPLACE FUNCTION ST_postgis_gist_joinsel(internal, oid, internal, smallint)

	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_gist_joinsel'
	LANGUAGE 'C';

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_overleft(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overleft'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_overleft(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overleft'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_overright(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overright'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_overright(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overright'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_overabove(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overabove'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_overabove(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overabove'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_overbelow(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overbelow'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_overbelow(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overbelow'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_left(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_left'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_left(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_left'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_right(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_right'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_right(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_right'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_above(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_above'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_above(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_above'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_below(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_below'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_below(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_below'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_contain(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_contain'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_contain(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_contain'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_contained(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_contained'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_contained(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_contained'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_overlap(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overlap'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_overlap(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_overlap'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry_same(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_same'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

--Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry_same(geometry, geometry) 
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_same'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- GEOMETRY operators

CREATE OPERATOR << (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_left,
   COMMUTATOR = '>>',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR &< (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_overleft,
   COMMUTATOR = '&>',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR <<| (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_below,
   COMMUTATOR = '|>>',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR &<| (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_overbelow,
   COMMUTATOR = '|&>',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR && (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_overlap,
   COMMUTATOR = '&&',
   RESTRICT = ST_postgis_gist_sel, JOIN = ST_postgis_gist_joinsel
);

CREATE OPERATOR &> (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_overright,
   COMMUTATOR = '&<',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR >> (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_right,
   COMMUTATOR = '<<',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR |&> (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_overabove,
   COMMUTATOR = '&<|',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR |>> (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_above,
   COMMUTATOR = '<<|',
   RESTRICT = positionsel, JOIN = positionjoinsel
);

CREATE OPERATOR ~= (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_same,
   COMMUTATOR = '~=', 
   RESTRICT = eqsel, JOIN = eqjoinsel
);

CREATE OPERATOR @ (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_contained,
   COMMUTATOR = '~',
   RESTRICT = contsel, JOIN = contjoinsel
);

CREATE OPERATOR ~ (
   LEFTARG = geometry, RIGHTARG = geometry, PROCEDURE = ST_geometry_contain,
   COMMUTATOR = '@',
   RESTRICT = contsel, JOIN = contjoinsel
);

-- gist support functions







CREATE OR REPLACE FUNCTION LWGEOM_gist_consistent(internal,geometry,int4) 
	RETURNS bool 
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_consistent'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_compress(internal) 
	RETURNS internal 
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_gist_compress'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_penalty(internal,internal,internal) 
	RETURNS internal 
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_penalty'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_picksplit(internal, internal) 
	RETURNS internal 
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_picksplit'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_union(bytea, internal) 
	RETURNS internal 
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_union'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_same(box2d, box2d, internal) 
	RETURNS internal 
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_same'
	LANGUAGE 'C';

CREATE OR REPLACE FUNCTION LWGEOM_gist_decompress(internal) 
	RETURNS internal
	AS '%{INSTALL_PATH}%/lib/liblwgeom' ,'LWGEOM_gist_decompress'
	LANGUAGE 'C';

-------------------------------------------
-- GIST opclass index binding entries.
-------------------------------------------


--
-- Create opclass index bindings for PG>=73
--

CREATE OPERATOR CLASS gist_geometry_ops
        DEFAULT FOR TYPE geometry USING gist AS
        OPERATOR        1        << 	RECHECK,
        OPERATOR        2        &<	RECHECK,
        OPERATOR        3        &&	RECHECK,
        OPERATOR        4        &>	RECHECK,
        OPERATOR        5        >>	RECHECK,
        OPERATOR        6        ~=	RECHECK,
        OPERATOR        7        ~	RECHECK,
        OPERATOR        8        @	RECHECK,
	OPERATOR	9	 &<|	RECHECK,
	OPERATOR	10	 <<|	RECHECK,
	OPERATOR	11	 |>>	RECHECK,
	OPERATOR	12	 |&>	RECHECK,
	FUNCTION        1        LWGEOM_gist_consistent (internal, geometry, int4),
        FUNCTION        2        LWGEOM_gist_union (bytea, internal),
        FUNCTION        3        LWGEOM_gist_compress (internal),
        FUNCTION        4        LWGEOM_gist_decompress (internal),
        FUNCTION        5        LWGEOM_gist_penalty (internal, internal, internal),
        FUNCTION        6        LWGEOM_gist_picksplit (internal, internal),
        FUNCTION        7        LWGEOM_gist_same (box2d, box2d, internal);

UPDATE pg_opclass 
	SET opckeytype = (SELECT oid FROM pg_type 
                          WHERE typname = 'box2d' 
                          AND typnamespace = (SELECT oid FROM pg_namespace 
                                              WHERE nspname=current_schema())) 
	WHERE opcname = 'gist_geometry_ops' 
        AND opcnamespace = (SELECT oid from pg_namespace 
                            WHERE nspname=current_schema());
	
-- TODO: add btree binding...


	
-------------------------------------------
-- other lwgeom functions
-------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION addBBOX(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_addBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_addBBOX(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_addBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION dropBBOX(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_dropBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_dropBBOX(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_dropBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);
	
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION getSRID(geometry) 
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_getSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION getSRID(geometry) 
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_getSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION getBBOX(geometry)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION getBBOX(geometry)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-------------------------------------------
--- CHIP functions
-------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION srid(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_srid(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION height(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getHeight'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_height(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getHeight'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION factor(chip)
	RETURNS FLOAT4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getFactor'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_factor(chip)
	RETURNS FLOAT4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getFactor'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION width(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getWidth'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_width(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getWidth'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION datatype(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getDatatype'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_datatype(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getDatatype'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION compression(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getCompression'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_compression(chip)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_getCompression'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION setSRID(chip,int4)
	RETURNS chip
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_setSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION setFactor(chip,float4)
	RETURNS chip
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_setFactor'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_setFactor(chip,float4)
	RETURNS chip
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_setFactor'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

------------------------------------------------------------------------
-- DEBUG
------------------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION mem_size(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_mem_size'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_mem_size(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_mem_size'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION summary(geometry)
	RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_summary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_summary(geometry)
	RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_summary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION npoints(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_npoints'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_npoints(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_npoints'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION nrings(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_nrings'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_nrings(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_nrings'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

------------------------------------------------------------------------
-- Misures
------------------------------------------------------------------------

-- this is a fake (for back-compatibility)
-- uses 3d if 3d is available, 2d otherwise
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION length3d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_length3d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION length2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length2d_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_length2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length2d_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE OR REPLACE FUNCTION length(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: length2d(geometry)
CREATE OR REPLACE FUNCTION ST_Length(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_length2d_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- this is a fake (for back-compatibility)
-- uses 3d if 3d is available, 2d otherwise
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION length3d_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_length3d_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION length_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_length_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION length2d_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length2d_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_length2d_spheroid(geometry, spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_length2d_ellipsoid_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- this is a fake (for back-compatibility)
-- uses 3d if 3d is available, 2d otherwise
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION perimeter3d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_perimeter3d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION perimeter2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter2d_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_perimeter2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter2d_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION perimeter(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: perimeter2d(geometry)
CREATE OR REPLACE FUNCTION ST_Perimeter(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_perimeter2d_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- this is an alias for 'area(geometry)'
-- there is nothing such an 'area3d'...
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION area2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_area_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_area2d(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_area_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION area(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_area_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: area(geometry)
CREATE OR REPLACE FUNCTION ST_Area(geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_area_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION distance_spheroid(geometry,geometry,spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_distance_ellipsoid_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_distance_spheroid(geometry,geometry,spheroid)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_distance_ellipsoid_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION distance_sphere(geometry,geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_distance_sphere'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_distance_sphere(geometry,geometry)
	RETURNS FLOAT8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_distance_sphere'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Minimum distance. 2d only.
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION distance(geometry,geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_mindistance2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: distance(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Distance(geometry,geometry)
    RETURNS float8
    AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_mindistance2d'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Maximum distance between linestrings. 2d only. Very bogus.
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION max_distance(geometry,geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_maxdistance2d_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_max_distance(geometry,geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_maxdistance2d_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION point_inside_circle(geometry,float8,float8,float8)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_inside_circle_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_point_inside_circle(geometry,float8,float8,float8)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_inside_circle_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION azimuth(geometry,geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_azimuth'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_azimuth(geometry,geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_azimuth'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

------------------------------------------------------------------------
-- MISC
------------------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_2d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_2d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_3dz(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dz'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_3dz(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dz'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- an alias for force_3dz
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_3d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dz'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_3d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dz'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_3dm(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dm'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_3dm(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_3dm'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_4d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_4d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_4d(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_4d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION force_collection(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_force_collection(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION multi(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_multi'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_multi(geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_force_multi'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION collector(geometry, geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect'
	LANGUAGE 'C' IMMUTABLE;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_collector(geometry, geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect'
	LANGUAGE 'C' IMMUTABLE;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION collect(geometry, geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect'
	LANGUAGE 'C' IMMUTABLE;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_collect(geometry, geometry) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect'
	LANGUAGE 'C' IMMUTABLE;

-- Deprecation in 1.2.3
CREATE AGGREGATE memcollect(
	sfunc = ST_collect,
	basetype = geometry,
	stype = geometry
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_memcollect(
	sfunc = ST_collect,
	basetype = geometry,
	stype = geometry
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geom_accum (geometry[],geometry)
	RETURNS geometry[]
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_accum'
	LANGUAGE 'C' IMMUTABLE;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geom_accum (geometry[],geometry)
	RETURNS geometry[]
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_accum'
	LANGUAGE 'C' IMMUTABLE;

-- Deprecation in 1.2.3
CREATE AGGREGATE accum (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[]
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_accum (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[]
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION collect_garray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_collect_garray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_collect_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE AGGREGATE collect (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_collect_garray
	);


-- Availability: 1.2.2
CREATE AGGREGATE ST_collect (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_collect_garray
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION expand(box3d,float8)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Expand(box3d,float8)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION expand(box2d,float8)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_expand(box2d,float8)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION expand(geometry,float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_expand(geometry,float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_expand'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION envelope(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_envelope'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- PostGIS equivalent function: envelope(geometry)
CREATE OR REPLACE FUNCTION ST_Envelope(geometry)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_envelope'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION reverse(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_reverse'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Reverse(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_reverse'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ForceRHR(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_forceRHR_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_ForceRHR(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_forceRHR_poly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION noop(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_noop'
	LANGUAGE 'C' VOLATILE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_noop(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_noop'
	LANGUAGE 'C' VOLATILE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION zmflag(geometry)
	RETURNS smallint
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_zmflag'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_zmflag(geometry)
	RETURNS smallint
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_zmflag'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION hasBBOX(geometry)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_hasBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availabitily: 1.2.2
CREATE OR REPLACE FUNCTION ST_HasBBOX(geometry)
	RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_hasBBOX'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ndims(geometry)
	RETURNS smallint
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_ndims'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_NDims(geometry)
	RETURNS smallint
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_ndims'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsEWKT(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asEWKT'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsEWKT(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asEWKT'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsEWKB(geometry)
	RETURNS BYTEA
	AS '%{INSTALL_PATH}%/lib/liblwgeom','WKBFromLWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsEWKB(geometry)
	RETURNS BYTEA
	AS '%{INSTALL_PATH}%/lib/liblwgeom','WKBFromLWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsHEXEWKB(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asHEXEWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsHEXEWKB(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asHEXEWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsHEXEWKB(geometry, text)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asHEXEWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsHEXEWKB(geometry, text)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asHEXEWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsEWKB(geometry,text)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','WKBFromLWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsEWKB(geometry,text)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','WKBFromLWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromEWKB(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOMFromWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomFromEWKB(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOMFromWKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromEWKT(text)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','parse_WKT_lwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomFromEWKT(text)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','parse_WKT_lwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION cache_bbox()
        RETURNS trigger
        AS '%{INSTALL_PATH}%/lib/liblwgeom'
        LANGUAGE 'C';

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Cache_BBox()
	RETURNS trigger
	AS '%{INSTALL_PATH}%/lib/liblwgeom','cache_bbox'
	LANGUAGE 'C';

------------------------------------------------------------------------
-- CONSTRUCTORS
------------------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePoint(float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakePoint(float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePoint(float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakePoint(float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePoint(float8, float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakePoint(float8, float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePointM(float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint3dm'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION MakePointM(float8, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint3dm'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakeBox2d(geometry, geometry)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_construct'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakeBox2d(geometry, geometry)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_construct'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakeBox3d(geometry, geometry)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_construct'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakeBox3d(geometry, geometry)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_construct'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION makeline_garray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makeline_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakeLine_GArray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makeline_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineFromMultiPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_from_mpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LineFromMultiPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_from_mpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakeLine(geometry, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makeline'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakeLine(geometry, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makeline'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AddPoint(geometry, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_addpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AddPoint(geometry, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_addpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AddPoint(geometry, geometry, integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_addpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AddPoint(geometry, geometry, integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_addpoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION RemovePoint(geometry, integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_removepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_RemovePoint(geometry, integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_removepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SetPoint(geometry, integer, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_setpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SetPoint(geometry, integer, geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_setpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE AGGREGATE makeline (
	sfunc = geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = makeline_garray
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_MakeLine (
	sfunc = geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_makeline_garray
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePolygon(geometry, geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakePolygon(geometry, geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MakePolygon(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MakePolygon(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoly'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION BuildArea(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_buildarea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_BuildArea(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_buildarea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Polygonize_GArray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'polygonize_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Polygonize_GArray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'polygonize_garray'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineMerge(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'linemerge'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LineMerge(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'linemerge'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE AGGREGATE Polygonize (
	sfunc = geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = polygonize_garray
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_Polygonize (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_polygonize_garray
	);



CREATE TYPE geometry_dump AS (path integer[], geom geometry);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Dump(geometry)
	RETURNS SETOF geometry_dump
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dump'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Dump(geometry)
	RETURNS SETOF geometry_dump
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dump'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION DumpRings(geometry)
	RETURNS SETOF geometry_dump
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dump_rings'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_DumpRings(geometry)
	RETURNS SETOF geometry_dump
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dump_rings'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);



------------------------------------------------------------------------

--
-- Aggregate functions
--

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION combine_bbox(box2d,geometry)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_combine'
	LANGUAGE 'C' IMMUTABLE;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Combine_BBox(box2d,geometry)
	RETURNS box2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX2DFLOAT4_combine'
	LANGUAGE 'C' IMMUTABLE;

-- Deprecation in 1.2.3
CREATE AGGREGATE Extent(
	sfunc = ST_combine_bbox,
	basetype = geometry,
	stype = box2d
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_Extent(
	sfunc = ST_combine_bbox,
	basetype = geometry,
	stype = box2d
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION combine_bbox(box3d,geometry)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_combine'
	LANGUAGE 'C' IMMUTABLE;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Combine_BBox(box3d,geometry)
	RETURNS box3d
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'BOX3D_combine'
	LANGUAGE 'C' IMMUTABLE;

-- Deprecation in 1.2.3
CREATE AGGREGATE Extent3d(
	sfunc = combine_bbox,
	basetype = geometry,
	stype = box3d
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_Extent3d(
	sfunc = ST_combine_bbox,
	basetype = geometry,
	stype = box3d
	);

-----------------------------------------------------------------------
-- CREATE_HISTOGRAM2D( <box2d>, <size> )
-----------------------------------------------------------------------
--
-- Returns a histgram with 0s in all the boxes.
--
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION create_histogram2d(box2d,int)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','create_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_create_histogram2d(box2d,int)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','create_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- BUILD_HISTOGRAM2D( <histogram2d>, <tablename>, <columnname> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION build_histogram2d (histogram2d,text,text)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','build_lwhistogram2d'
	LANGUAGE 'C' STABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_build_histogram2d (histogram2d,text,text)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','build_lwhistogram2d'
	LANGUAGE 'C' STABLE STRICT; -- WITH (isstrict);


-----------------------------------------------------------------------
-- BUILD_HISTOGRAM2D(<histogram2d>,<schema>,<tablename>,<columnname>)
-----------------------------------------------------------------------
-- This is a wrapper to the omonimous schema unaware function,
-- thanks to Carl Anderson for the idea.
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION build_histogram2d (histogram2d,text,text,text)
RETURNS histogram2d
AS '
BEGIN
	EXECUTE ''SET local search_path = ''||$2||'',public'';
	RETURN public.build_histogram2d($1,$3,$4);
END
'
LANGUAGE 'plpgsql' STABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_build_histogram2d (histogram2d,text,text,text)
RETURNS histogram2d
AS '
BEGIN
	EXECUTE ''SET local search_path = ''||$2||'',public'';
	RETURN public.build_histogram2d($1,$3,$4);
END
'
LANGUAGE 'plpgsql' STABLE STRICT; -- WITH (isstrict);



-----------------------------------------------------------------------
-- EXPLODE_HISTOGRAM2D( <histogram2d>, <tablename> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION explode_histogram2d (histogram2d,text)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','explode_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_explode_histogram2d (histogram2d,text)
	RETURNS histogram2d
	AS '%{INSTALL_PATH}%/lib/liblwgeom','explode_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- ESTIMATE_HISTOGRAM2D( <histogram2d>, <box> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION estimate_histogram2d(histogram2d,box2d)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','estimate_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_estimate_histogram2d(histogram2d,box2d)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','estimate_lwhistogram2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- ESTIMATED_EXTENT( <schema name>, <table name>, <column name> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION estimated_extent(text,text,text) RETURNS box2d AS
	'%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_estimated_extent'
	LANGUAGE 'C' IMMUTABLE STRICT SECURITY DEFINER;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_estimated_extent(text,text,text) RETURNS box2d AS
	'%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_estimated_extent'
	LANGUAGE 'C' IMMUTABLE STRICT SECURITY DEFINER;

-----------------------------------------------------------------------
-- ESTIMATED_EXTENT( <table name>, <column name> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION estimated_extent(text,text) RETURNS box2d AS
	'%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_estimated_extent'
	LANGUAGE 'C' IMMUTABLE STRICT SECURITY DEFINER; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_estimated_extent(text,text) RETURNS box2d AS
	'%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_estimated_extent'
	LANGUAGE 'C' IMMUTABLE STRICT SECURITY DEFINER; 

-----------------------------------------------------------------------
-- FIND_EXTENT( <schema name>, <table name>, <column name> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION find_extent(text,text,text) RETURNS box2d AS
'
DECLARE
	schemaname alias for $1;
	tablename alias for $2;
	columnname alias for $3;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE ''SELECT extent("''||columnname||''") FROM "''||schemaname||''"."''||tablename||''"'' LOOP
		return myrec.extent;
	END LOOP; 
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_find_extent(text,text,text) RETURNS box2d AS
'
DECLARE
	schemaname alias for $1;
	tablename alias for $2;
	columnname alias for $3;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE ''SELECT extent("''||columnname||''") FROM "''||schemaname||''"."''||tablename||''"'' LOOP
		return myrec.extent;
	END LOOP; 
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (isstrict);


-----------------------------------------------------------------------
-- FIND_EXTENT( <table name>, <column name> )
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION find_extent(text,text) RETURNS box2d AS
'
DECLARE
	tablename alias for $1;
	columnname alias for $2;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE ''SELECT extent("''||columnname||''") FROM "''||tablename||''"'' LOOP
		return myrec.extent;
	END LOOP; 
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_find_extent(text,text) RETURNS box2d AS
'
DECLARE
	tablename alias for $1;
	columnname alias for $2;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE ''SELECT extent("''||columnname||''") FROM "''||tablename||''"'' LOOP
		return myrec.extent;
	END LOOP; 
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (isstrict);

-------------------------------------------------------------------
-- SPATIAL_REF_SYS
-------------------------------------------------------------------
CREATE TABLE spatial_ref_sys (
	 srid integer not null primary key,
	 auth_name varchar(256), 
	 auth_srid integer, 
	 srtext varchar(2048),
	 proj4text varchar(2048) 
);

-------------------------------------------------------------------
-- GEOMETRY_COLUMNS
-------------------------------------------------------------------
CREATE TABLE geometry_columns (
	f_table_catalog varchar(256) not null,
	f_table_schema varchar(256) not null,
	f_table_name varchar(256) not null,
	f_geometry_column varchar(256) not null,
	coord_dimension integer not null,
	srid integer not null,
	type varchar(30) not null,





	CONSTRAINT geometry_columns_pk primary key ( 
		f_table_catalog, 
		f_table_schema, 
		f_table_name, 
		f_geometry_column )
) WITH OIDS;

-----------------------------------------------------------------------
-- RENAME_GEOMETRY_TABLE_CONSTRAINTS()
-----------------------------------------------------------------------
-- This function has been obsoleted for the difficulty in
-- finding attribute on which the constraint is applied.
-- AddGeometryColumn will name the constraints in a meaningful
-- way, but nobody can rely on it since old postgis versions did
-- not do that.
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION rename_geometry_table_constraints() RETURNS text
AS 
'
SELECT ''rename_geometry_table_constraint() is obsoleted''::text
'
LANGUAGE 'SQL' IMMUTABLE;

-----------------------------------------------------------------------
-- FIX_GEOMETRY_COLUMNS() 
-----------------------------------------------------------------------
-- This function will:
--
--	o try to fix the schema of records with an invalid one
--		(for PG>=73)
--
--	o link records to system tables through attrelid and varattnum
--		(for PG<75)
--
--	o delete all records for which no linking was possible
--		(for PG<75)
--	
-- 
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION fix_geometry_columns() RETURNS text
AS 
'
DECLARE
	mislinked record;
	result text;
	linked integer;
	deleted integer;

	foundschema integer;

BEGIN


	-- Since 7.3 schema support has been added.
	-- Previous postgis versions used to put the database name in
	-- the schema column. This needs to be fixed, so we try to 
	-- set the correct schema for each geometry_colums record
	-- looking at table, column, type and srid.
	UPDATE geometry_columns SET f_table_schema = n.nspname
		FROM pg_namespace n, pg_class c, pg_attribute a,
			pg_constraint sridcheck, pg_constraint typecheck
                WHERE ( f_table_schema is NULL
		OR f_table_schema = ''''
                OR f_table_schema NOT IN (
                        SELECT nspname::varchar
                        FROM pg_namespace nn, pg_class cc, pg_attribute aa
                        WHERE cc.relnamespace = nn.oid
                        AND cc.relname = f_table_name::name
                        AND aa.attrelid = cc.oid
                        AND aa.attname = f_geometry_column::name))
                AND f_table_name::name = c.relname
                AND c.oid = a.attrelid
                AND c.relnamespace = n.oid
                AND f_geometry_column::name = a.attname

                AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE ''(srid(% = %)''
                AND sridcheck.consrc ~ textcat('' = '', srid::text)

                AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
	''((geometrytype(%) = ''''%''''::text) OR (% IS NULL))''
                AND typecheck.consrc ~ textcat('' = '''''', type::text)

                AND NOT EXISTS (
                        SELECT oid FROM geometry_columns gc
                        WHERE c.relname::varchar = gc.f_table_name
                        AND n.nspname::varchar = gc.f_table_schema
                        AND a.attname::varchar = gc.f_geometry_column
                );

	GET DIAGNOSTICS foundschema = ROW_COUNT;



	-- no linkage to system table needed
	return ''fixed:''||foundschema::text;


	-- fix linking to system tables
	SELECT 0 INTO linked;
	FOR mislinked in
		SELECT gc.oid as gcrec,
			a.attrelid as attrelid, a.attnum as attnum
                FROM geometry_columns gc, pg_class c,

		pg_namespace n, pg_attribute a



                WHERE ( gc.attrelid IS NULL OR gc.attrelid != a.attrelid 
			OR gc.varattnum IS NULL OR gc.varattnum != a.attnum)

                AND n.nspname = gc.f_table_schema::name
                AND c.relnamespace = n.oid

                AND c.relname = gc.f_table_name::name
                AND a.attname = f_geometry_column::name
                AND a.attrelid = c.oid
	LOOP
		UPDATE geometry_columns SET
			attrelid = mislinked.attrelid,
			varattnum = mislinked.attnum,
			stats = NULL
			WHERE geometry_columns.oid = mislinked.gcrec;
		SELECT linked+1 INTO linked;
	END LOOP; 

	-- remove stale records
	DELETE FROM geometry_columns WHERE attrelid IS NULL;

	GET DIAGNOSTICS deleted = ROW_COUNT;

	result = 

		''fixed:'' || foundschema::text ||

		'' linked:'' || linked::text || 
		'' deleted:'' || deleted::text;

	return result;

END;
'
LANGUAGE 'plpgsql' VOLATILE;

-----------------------------------------------------------------------
-- PROBE_GEOMETRY_COLUMNS() 
-----------------------------------------------------------------------
-- Fill the geometry_columns table with values probed from the system
-- catalogues. 3d flag cannot be probed, it defaults to 2
--
-- Note that bogus records already in geometry_columns are not
-- overridden (a check for schema.table.column is performed), so
-- to have a fresh probe backup your geometry_column, delete from
-- it and probe.
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION probe_geometry_columns() RETURNS text AS
'
DECLARE
	inserted integer;
	oldcount integer;
	probed integer;
	stale integer;
BEGIN

	SELECT count(*) INTO oldcount FROM geometry_columns;

	SELECT count(*) INTO probed
		FROM pg_class c, pg_attribute a, pg_type t, 

			pg_namespace n,
			pg_constraint sridcheck, pg_constraint typecheck




		WHERE t.typname = ''geometry''
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid

		AND c.relnamespace = n.oid
		AND sridcheck.connamespace = n.oid
		AND typecheck.connamespace = n.oid



		AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE ''(srid(''||a.attname||'') = %)''
		AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
	''((geometrytype(''||a.attname||'') = ''''%''''::text) OR (% IS NULL))''







		;

	INSERT INTO geometry_columns SELECT
		''''::varchar as f_table_catalogue,

		n.nspname::varchar as f_table_schema,



		c.relname::varchar as f_table_name,
		a.attname::varchar as f_geometry_column,
		2 as coord_dimension,

		trim(both  '' =)'' from substr(sridcheck.consrc,
			strpos(sridcheck.consrc, ''='')))::integer as srid,
		trim(both '' =)'''''' from substr(typecheck.consrc, 
			strpos(typecheck.consrc, ''=''),
			strpos(typecheck.consrc, ''::'')-
			strpos(typecheck.consrc, ''='')
			))::varchar as type






		FROM pg_class c, pg_attribute a, pg_type t, 

			pg_namespace n,
			pg_constraint sridcheck, pg_constraint typecheck



		WHERE t.typname = ''geometry''
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid

		AND c.relnamespace = n.oid
		AND sridcheck.connamespace = n.oid
		AND typecheck.connamespace = n.oid
		AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE ''(srid(''||a.attname||'') = %)''
		AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
	''((geometrytype(''||a.attname||'') = ''''%''''::text) OR (% IS NULL))''








                AND NOT EXISTS (
                        SELECT oid FROM geometry_columns gc
                        WHERE c.relname::varchar = gc.f_table_name

                        AND n.nspname::varchar = gc.f_table_schema

                        AND a.attname::varchar = gc.f_geometry_column
                );

	GET DIAGNOSTICS inserted = ROW_COUNT;

	IF oldcount > probed THEN
		stale = oldcount-probed;
	ELSE
		stale = 0;
	END IF;

        RETURN ''probed:''||probed::text||
		'' inserted:''||inserted::text||
		'' conflicts:''||(probed-inserted)::text||
		'' stale:''||stale::text;
END

'
LANGUAGE 'plpgsql' VOLATILE;

-----------------------------------------------------------------------
-- ADDGEOMETRYCOLUMN
--   <catalogue>, <schema>, <table>, <column>, <srid>, <type>, <dim>
-----------------------------------------------------------------------
--
-- Type can be one of geometry, GEOMETRYCOLLECTION, POINT, MULTIPOINT, POLYGON,
-- MULTIPOLYGON, LINESTRING, or MULTILINESTRING.
--
-- Types (except geometry) are checked for consistency using a CHECK constraint
-- uses SQL ALTER TABLE command to add the geometry column to the table.
-- Addes a row to geometry_columns.
-- Addes a constraint on the table that all the geometries MUST have the same 
-- SRID. Checks the coord_dimension to make sure its between 0 and 3.
-- Should also check the precision grid (future expansion).
-- Calls fix_geometry_columns() at the end.
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION AddGeometryColumn(varchar,varchar,varchar,varchar,integer,varchar,integer)
	RETURNS text
	AS 
'
DECLARE
	catalog_name alias for $1;
	schema_name alias for $2;
	table_name alias for $3;
	column_name alias for $4;
	new_srid alias for $5;
	new_type alias for $6;
	new_dim alias for $7;

	rec RECORD;
	schema_ok bool;
	real_schema name;


BEGIN

	IF ( not ( (new_type =''GEOMETRY'') or
		   (new_type =''GEOMETRYCOLLECTION'') or
		   (new_type =''POINT'') or 
		   (new_type =''MULTIPOINT'') or
		   (new_type =''POLYGON'') or
		   (new_type =''MULTIPOLYGON'') or
		   (new_type =''LINESTRING'') or
		   (new_type =''MULTILINESTRING'') or
		   (new_type =''GEOMETRYCOLLECTIONM'') or
		   (new_type =''POINTM'') or 
		   (new_type =''MULTIPOINTM'') or
		   (new_type =''POLYGONM'') or
		   (new_type =''MULTIPOLYGONM'') or
		   (new_type =''LINESTRINGM'') or
		   (new_type =''MULTILINESTRINGM'') or
                   (new_type = ''CIRCULARSTRING'') or
                   (new_type = ''CIRCULARSTRINGM'') or
                   (new_type = ''COMPOUNDCURVE'') or
                   (new_type = ''COMPOUNDCURVEM'') or
                   (new_type = ''CURVEPOLYGON'') or
                   (new_type = ''CURVEPOLYGONM'') or
                   (new_type = ''MULTICURVE'') or
                   (new_type = ''MULTICURVEM'') or
                   (new_type = ''MULTISURFACE'') or
                   (new_type = ''MULTISURFACEM'')) )
	THEN
		RAISE EXCEPTION ''Invalid type name - valid ones are: 
			GEOMETRY, GEOMETRYCOLLECTION, POINT, 
			MULTIPOINT, POLYGON, MULTIPOLYGON, 
			LINESTRING, MULTILINESTRING,
                        CIRCULARSTRING, COMPOUNDCURVE,
                        CURVEPOLYGON, MULTICURVE, MULTISURFACE,
			GEOMETRYCOLLECTIONM, POINTM, 
			MULTIPOINTM, POLYGONM, MULTIPOLYGONM, 
			LINESTRINGM, MULTILINESTRINGM 
                        CIRCULARSTRINGM, COMPOUNDCURVEM,
                        CURVEPOLYGONM, MULTICURVEM or MULTISURFACEM'';
		return ''fail'';
	END IF;

	IF ( (new_dim >4) or (new_dim <0) ) THEN
		RAISE EXCEPTION ''invalid dimension'';
		return ''fail'';
	END IF;

	IF ( (new_type LIKE ''%M'') and (new_dim!=3) ) THEN

		RAISE EXCEPTION ''TypeM needs 3 dimensions'';
		return ''fail'';
	END IF;


	IF ( schema_name != '''' ) THEN
		schema_ok = ''f'';
		FOR rec IN SELECT nspname FROM pg_namespace WHERE text(nspname) = schema_name LOOP
			schema_ok := ''t'';
		END LOOP;

		if ( schema_ok <> ''t'' ) THEN
			RAISE NOTICE ''Invalid schema name - using current_schema()'';
			SELECT current_schema() into real_schema;
		ELSE
			real_schema = schema_name;
		END IF;

	ELSE
		SELECT current_schema() into real_schema;
	END IF;



	-- Add geometry column

	EXECUTE ''ALTER TABLE '' ||

		quote_ident(real_schema) || ''.'' || quote_ident(table_name)



		|| '' ADD COLUMN '' || quote_ident(column_name) || 
		'' geometry '';


	-- Delete stale record in geometry_column (if any)

	EXECUTE ''DELETE FROM geometry_columns WHERE
		f_table_catalog = '' || quote_literal('''') || 
		'' AND f_table_schema = '' ||

		quote_literal(real_schema) || 



		'' AND f_table_name = '' || quote_literal(table_name) ||
		'' AND f_geometry_column = '' || quote_literal(column_name);


	-- Add record in geometry_column 

	EXECUTE ''INSERT INTO geometry_columns VALUES ('' ||
		quote_literal('''') || '','' ||

		quote_literal(real_schema) || '','' ||



		quote_literal(table_name) || '','' ||
		quote_literal(column_name) || '','' ||
		new_dim::text || '','' || new_srid::text || '','' ||
		quote_literal(new_type) || '')'';

	-- Add table checks

	EXECUTE ''ALTER TABLE '' || 

		quote_ident(real_schema) || ''.'' || quote_ident(table_name)



		|| '' ADD CONSTRAINT '' 
		|| quote_ident(''enforce_srid_'' || column_name)
		|| '' CHECK (SRID('' || quote_ident(column_name) ||
		'') = '' || new_srid::text || '')'' ;

	EXECUTE ''ALTER TABLE '' || 

		quote_ident(real_schema) || ''.'' || quote_ident(table_name)



		|| '' ADD CONSTRAINT ''
		|| quote_ident(''enforce_dims_'' || column_name)
		|| '' CHECK (ndims('' || quote_ident(column_name) ||
		'') = '' || new_dim::text || '')'' ;

	IF (not(new_type = ''GEOMETRY'')) THEN
		EXECUTE ''ALTER TABLE '' || 

		quote_ident(real_schema) || ''.'' || quote_ident(table_name)



		|| '' ADD CONSTRAINT ''
		|| quote_ident(''enforce_geotype_'' || column_name)
		|| '' CHECK (geometrytype('' ||
		quote_ident(column_name) || '')='' ||
		quote_literal(new_type) || '' OR ('' ||
		quote_ident(column_name) || '') is null)'';
	END IF;

	return 

		real_schema || ''.'' || 

		table_name || ''.'' || column_name ||
		'' SRID:'' || new_srid::text ||
		'' TYPE:'' || new_type || 
		'' DIMS:'' || new_dim::text || chr(10) || '' ''; 
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

----------------------------------------------------------------------------
-- ADDGEOMETRYCOLUMN ( <schema>, <table>, <column>, <srid>, <type>, <dim> )
----------------------------------------------------------------------------
--
-- This is a wrapper to the real AddGeometryColumn, for use
-- when catalogue is undefined
--
----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION AddGeometryColumn(varchar,varchar,varchar,integer,varchar,integer) RETURNS text AS '
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('''',$1,$2,$3,$4,$5,$6) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' STABLE STRICT; -- WITH (isstrict);

----------------------------------------------------------------------------
-- ADDGEOMETRYCOLUMN ( <table>, <column>, <srid>, <type>, <dim> )
----------------------------------------------------------------------------
--
-- This is a wrapper to the real AddGeometryColumn, for use
-- when catalogue and schema are undefined
--
----------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION AddGeometryColumn(varchar,varchar,integer,varchar,integer) RETURNS text AS '
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('''','''',$1,$2,$3,$4,$5) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYCOLUMN
--   <catalogue>, <schema>, <table>, <column>
-----------------------------------------------------------------------
--
-- Removes geometry column reference from geometry_columns table.
-- Drops the column with pgsql >= 73.
-- Make some silly enforcements on it for pgsql < 73
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryColumn(varchar, varchar,varchar,varchar)
	RETURNS text
	AS 
'
DECLARE
	catalog_name alias for $1; 
	schema_name alias for $2;
	table_name alias for $3;
	column_name alias for $4;
	myrec RECORD;
	okay boolean;
	real_schema name;

BEGIN



	-- Find, check or fix schema_name
	IF ( schema_name != '''' ) THEN
		okay = ''f'';

		FOR myrec IN SELECT nspname FROM pg_namespace WHERE text(nspname) = schema_name LOOP
			okay := ''t'';
		END LOOP;

		IF ( okay <> ''t'' ) THEN
			RAISE NOTICE ''Invalid schema name - using current_schema()'';
			SELECT current_schema() into real_schema;
		ELSE
			real_schema = schema_name;
		END IF;
	ELSE
		SELECT current_schema() into real_schema;
	END IF;




 	-- Find out if the column is in the geometry_columns table
	okay = ''f'';
	FOR myrec IN SELECT * from geometry_columns where f_table_schema = text(real_schema) and f_table_name = table_name and f_geometry_column = column_name LOOP
		okay := ''t'';
	END LOOP; 
	IF (okay <> ''t'') THEN 
		RAISE EXCEPTION ''column not found in geometry_columns table'';
		RETURN ''f'';
	END IF;

	-- Remove ref from geometry_columns table
	EXECUTE ''delete from geometry_columns where f_table_schema = '' ||
		quote_literal(real_schema) || '' and f_table_name = '' ||
		quote_literal(table_name)  || '' and f_geometry_column = '' ||
		quote_literal(column_name);
	
	-- Remove table column
	EXECUTE ''ALTER TABLE '' || quote_ident(real_schema) || ''.'' ||
		quote_ident(table_name) || '' DROP COLUMN '' ||
		quote_ident(column_name);



	RETURN real_schema || ''.'' || table_name || ''.'' || column_name ||'' effectively removed.'';
	
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYCOLUMN
--   <schema>, <table>, <column>
-----------------------------------------------------------------------
--
-- This is a wrapper to the real DropGeometryColumn, for use
-- when catalogue is undefined
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryColumn(varchar,varchar,varchar)
	RETURNS text
	AS 
'
DECLARE
	ret text;
BEGIN
	SELECT DropGeometryColumn('''',$1,$2,$3) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYCOLUMN
--   <table>, <column>
-----------------------------------------------------------------------
--
-- This is a wrapper to the real DropGeometryColumn, for use
-- when catalogue and schema is undefined. 
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryColumn(varchar,varchar)
	RETURNS text
	AS 
'
DECLARE
	ret text;
BEGIN
	SELECT DropGeometryColumn('''','''',$1,$2) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYTABLE
--   <catalogue>, <schema>, <table>
-----------------------------------------------------------------------
--
-- Drop a table and all its references in geometry_columns
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryTable(varchar, varchar,varchar)
	RETURNS text
	AS 
'
DECLARE
	catalog_name alias for $1; 
	schema_name alias for $2;
	table_name alias for $3;
	real_schema name;

BEGIN


	IF ( schema_name = '''' ) THEN
		SELECT current_schema() into real_schema;
	ELSE
		real_schema = schema_name;
	END IF;


	-- Remove refs from geometry_columns table
	EXECUTE ''DELETE FROM geometry_columns WHERE '' ||

		''f_table_schema = '' || quote_literal(real_schema) ||
		'' AND '' ||

		'' f_table_name = '' || quote_literal(table_name);
	
	-- Remove table 
	EXECUTE ''DROP TABLE ''

		|| quote_ident(real_schema) || ''.'' ||

		quote_ident(table_name);

	RETURN

		real_schema || ''.'' ||

		table_name ||'' dropped.'';
	
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYTABLE
--   <schema>, <table>
-----------------------------------------------------------------------
--
-- Drop a table and all its references in geometry_columns
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryTable(varchar,varchar) RETURNS text AS 
'SELECT DropGeometryTable('''',$1,$2)'
LANGUAGE 'sql' WITH (isstrict);

-----------------------------------------------------------------------
-- DROPGEOMETRYTABLE
--   <table>
-----------------------------------------------------------------------
--
-- Drop a table and all its references in geometry_columns
-- For PG>=73 use current_schema()
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION DropGeometryTable(varchar) RETURNS text AS 
'SELECT DropGeometryTable('''','''',$1)'
LANGUAGE 'sql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- UPDATEGEOMETRYSRID
--   <catalogue>, <schema>, <table>, <column>, <srid>
-----------------------------------------------------------------------
--
-- Change SRID of all features in a spatially-enabled table
--
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION UpdateGeometrySRID(varchar,varchar,varchar,varchar,integer)
	RETURNS text
	AS 
'
DECLARE
	catalog_name alias for $1; 
	schema_name alias for $2;
	table_name alias for $3;
	column_name alias for $4;
	new_srid alias for $5;
	myrec RECORD;
	okay boolean;
	cname varchar;
	real_schema name;

BEGIN



	-- Find, check or fix schema_name
	IF ( schema_name != '''' ) THEN
		okay = ''f'';

		FOR myrec IN SELECT nspname FROM pg_namespace WHERE text(nspname) = schema_name LOOP
			okay := ''t'';
		END LOOP;

		IF ( okay <> ''t'' ) THEN
			RAISE EXCEPTION ''Invalid schema name'';
		ELSE
			real_schema = schema_name;
		END IF;
	ELSE
		SELECT INTO real_schema current_schema()::text;
	END IF;


 	-- Find out if the column is in the geometry_columns table
	okay = ''f'';
	FOR myrec IN SELECT * from geometry_columns where f_table_schema = text(real_schema) and f_table_name = table_name and f_geometry_column = column_name LOOP
		okay := ''t'';
	END LOOP; 
	IF (okay <> ''t'') THEN 
		RAISE EXCEPTION ''column not found in geometry_columns table'';
		RETURN ''f'';
	END IF;

	-- Update ref from geometry_columns table
	EXECUTE ''UPDATE geometry_columns SET SRID = '' || new_srid::text || 
		'' where f_table_schema = '' ||
		quote_literal(real_schema) || '' and f_table_name = '' ||
		quote_literal(table_name)  || '' and f_geometry_column = '' ||
		quote_literal(column_name);
	
	-- Make up constraint name
	cname = ''enforce_srid_''  || column_name;

	-- Drop enforce_srid constraint



	EXECUTE ''ALTER TABLE '' || quote_ident(real_schema) ||
		''.'' || quote_ident(table_name) ||

		'' DROP constraint '' || quote_ident(cname);

	-- Update geometries SRID



	EXECUTE ''UPDATE '' || quote_ident(real_schema) ||
		''.'' || quote_ident(table_name) ||

		'' SET '' || quote_ident(column_name) ||
		'' = setSRID('' || quote_ident(column_name) ||
		'', '' || new_srid::text || '')'';

	-- Reset enforce_srid constraint



	EXECUTE ''ALTER TABLE '' || quote_ident(real_schema) ||
		''.'' || quote_ident(table_name) ||

		'' ADD constraint '' || quote_ident(cname) ||
		'' CHECK (srid('' || quote_ident(column_name) ||
		'') = '' || new_srid::text || '')'';

	RETURN real_schema || ''.'' || table_name || ''.'' || column_name ||'' SRID changed to '' || new_srid::text;
	
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- UPDATEGEOMETRYSRID
--   <schema>, <table>, <column>, <srid>
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION UpdateGeometrySRID(varchar,varchar,varchar,integer)
	RETURNS text
	AS '
DECLARE
	ret  text;
BEGIN
	SELECT UpdateGeometrySRID('''',$1,$2,$3,$4) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- UPDATEGEOMETRYSRID
--   <table>, <column>, <srid>
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION UpdateGeometrySRID(varchar,varchar,integer)
	RETURNS text
	AS '
DECLARE
	ret  text;
BEGIN
	SELECT UpdateGeometrySRID('''','''',$1,$2,$3) into ret;
	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT; -- WITH (isstrict);

-----------------------------------------------------------------------
-- UPDATE_GEOMETRY_STATS()
-----------------------------------------------------------------------
--
-- Only meaningful for PG<75.
-- Gather statisticts about geometry columns for use
-- with cost estimator.
--
-- It is defined also for PG>=75 for back-compatibility
--
-----------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_geometry_stats() RETURNS text
AS ' SELECT ''update_geometry_stats() has been obsoleted. Statistics are automatically built running the ANALYZE command''::text' LANGUAGE 'sql';

-----------------------------------------------------------------------
-- UPDATE_GEOMETRY_STATS( <table>, <column> )
-----------------------------------------------------------------------
--
-- Only meaningful for PG<75.
-- Gather statisticts about a geometry column for use
-- with cost estimator.
--
-- It is defined also for PG>=75 for back-compatibility
--
-----------------------------------------------------------------------

CREATE OR REPLACE FUNCTION update_geometry_stats(varchar,varchar) RETURNS text
AS 'SELECT update_geometry_stats();' LANGUAGE 'sql' ;

-----------------------------------------------------------------------
-- FIND_SRID( <schema>, <table>, <geom col> )
-----------------------------------------------------------------------
CREATE OR REPLACE FUNCTION find_srid(varchar,varchar,varchar) RETURNS int4 AS
'DECLARE
   schem text;
   tabl text;
   sr int4;
BEGIN
   IF $1 IS NULL THEN
      RAISE EXCEPTION ''find_srid() - schema is NULL!'';
   END IF;
   IF $2 IS NULL THEN
      RAISE EXCEPTION ''find_srid() - table name is NULL!'';
   END IF;
   IF $3 IS NULL THEN
      RAISE EXCEPTION ''find_srid() - column name is NULL!'';
   END IF;
   schem = $1;
   tabl = $2;
-- if the table contains a . and the schema is empty
-- split the table into a schema and a table
-- otherwise drop through to default behavior
   IF ( schem = '''' and tabl LIKE ''%.%'' ) THEN
     schem = substr(tabl,1,strpos(tabl,''.'')-1);
     tabl = substr(tabl,length(schem)+2);
   ELSE
     schem = schem || ''%'';
   END IF;

   select SRID into sr from geometry_columns where f_table_schema like schem and f_table_name = tabl and f_geometry_column = $3;
   IF NOT FOUND THEN
       RAISE EXCEPTION ''find_srid() - couldnt find the corresponding SRID - is the geometry registered in the GEOMETRY_COLUMNS table?  Is there an uppercase/lowercase missmatch?'';
   END IF;
  return sr;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (iscachable); 


---------------------------------------------------------------
-- PROJ support
---------------------------------------------------------------

CREATE OR REPLACE FUNCTION get_proj4_from_srid(integer) RETURNS text AS
'
BEGIN
	RETURN proj4text::text FROM spatial_ref_sys WHERE srid= $1;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);



CREATE OR REPLACE FUNCTION transform_geometry(geometry,text,text,int)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','transform_geom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

CREATE OR REPLACE FUNCTION transform(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','transform'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: transform(geometry,integer)
CREATE OR REPLACE FUNCTION ST_Transform(geometry,integer)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','transform'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-----------------------------------------------------------------------
-- POSTGIS_VERSION()
-----------------------------------------------------------------------

CREATE OR REPLACE FUNCTION postgis_version() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_proj_version() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

--
-- IMPORTANT:
-- Starting at 1.1.0 this function is used by postgis_proc_upgrade.pl
-- to extract version of postgis being installed.
-- Do not modify this w/out also changing postgis_proc_upgrade.pl
--
CREATE OR REPLACE FUNCTION postgis_scripts_installed() RETURNS text
        AS 'SELECT ''1.3.3''::text AS version'
        LANGUAGE 'sql' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_lib_version() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE; -- a new lib will require a new session

-- NOTE: starting at 1.1.0 this is the same of postgis_lib_version()
CREATE OR REPLACE FUNCTION postgis_scripts_released() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_uses_stats() RETURNS bool
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_geos_version() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_jts_version() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_scripts_build_date() RETURNS text
        AS 'SELECT ''2008-10-30 13:59:14''::text AS version'
        LANGUAGE 'sql' IMMUTABLE;

CREATE OR REPLACE FUNCTION postgis_lib_build_date() RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE;



CREATE OR REPLACE FUNCTION postgis_full_version() RETURNS text
AS '
DECLARE
	libver text;
	projver text;
	geosver text;
	jtsver text;
	usestats bool;
	dbproc text;
	relproc text;
	fullver text;
BEGIN
	SELECT postgis_lib_version() INTO libver;
	SELECT postgis_proj_version() INTO projver;
	SELECT postgis_geos_version() INTO geosver;
	SELECT postgis_jts_version() INTO jtsver;
	SELECT postgis_uses_stats() INTO usestats;
	SELECT postgis_scripts_installed() INTO dbproc;
	SELECT postgis_scripts_released() INTO relproc;

	fullver = ''POSTGIS="'' || libver || ''"'';

	IF  geosver IS NOT NULL THEN
		fullver = fullver || '' GEOS="'' || geosver || ''"'';
	END IF;

	IF  jtsver IS NOT NULL THEN
		fullver = fullver || '' JTS="'' || jtsver || ''"'';
	END IF;

	IF  projver IS NOT NULL THEN
		fullver = fullver || '' PROJ="'' || projver || ''"'';
	END IF;

	IF usestats THEN
		fullver = fullver || '' USE_STATS'';
	END IF;

	-- fullver = fullver || '' DBPROC="'' || dbproc || ''"'';
	-- fullver = fullver || '' RELPROC="'' || relproc || ''"'';

	IF dbproc != relproc THEN
		fullver = fullver || '' (procs from '' || dbproc || '' need upgrade)'';
	END IF;

	RETURN fullver;
END
'
LANGUAGE 'plpgsql' IMMUTABLE;

---------------------------------------------------------------
-- CASTS
---------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d(geometry)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d(geometry)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box3d(geometry)
        RETURNS box3d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX3D'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box3d(geometry)
        RETURNS box3d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX3D'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box(geometry)
        RETURNS box
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box(geometry)
        RETURNS box
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_BOX'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box2d(box3d)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box2d(box3d)
        RETURNS box2d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_BOX2DFLOAT4'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box3d(box2d)
        RETURNS box3d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_to_BOX3D'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box3d(box2d)
        RETURNS box3d
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_to_BOX3D'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box(box3d)
        RETURNS box
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_BOX'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_box(box3d)
        RETURNS box
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_BOX'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION text(geometry)
        RETURNS text
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_text(geometry)
        RETURNS text
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- this is kept for backward-compatibility
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION box3dtobox(box3d)
        RETURNS box
        AS 'SELECT box($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry(box2d)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_to_LWGEOM'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry(box2d)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX2DFLOAT4_to_LWGEOM'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry(box3d)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_LWGEOM'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry(box3d)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','BOX3D_to_LWGEOM'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry(text)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','parse_WKT_lwgeom'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry(text)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','parse_WKT_lwgeom'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry(chip)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_to_LWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry(chip)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','CHIP_to_LWGEOM'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION geometry(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_bytea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_geometry(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_bytea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION bytea(geometry)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_bytea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_bytea(geometry)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_to_bytea'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION text(bool)
	RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOOL_to_text'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_text(bool)
	RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom','BOOL_to_text'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- 7.3+ explicit casting definitions

CREATE CAST (geometry AS box2d) WITH FUNCTION ST_box2d(geometry) AS IMPLICIT;
CREATE CAST (geometry AS box3d) WITH FUNCTION ST_box3d(geometry) AS IMPLICIT;
CREATE CAST (geometry AS box) WITH FUNCTION ST_box(geometry) AS IMPLICIT;
CREATE CAST (box3d AS box2d) WITH FUNCTION ST_box2d(box3d) AS IMPLICIT;
CREATE CAST (box2d AS box3d) WITH FUNCTION ST_box3d(box2d) AS IMPLICIT;
CREATE CAST (box2d AS geometry) WITH FUNCTION ST_geometry(box2d) AS IMPLICIT;
CREATE CAST (box3d AS box) WITH FUNCTION ST_box(box3d) AS IMPLICIT;
CREATE CAST (box3d AS geometry) WITH FUNCTION ST_geometry(box3d) AS IMPLICIT;
CREATE CAST (text AS geometry) WITH FUNCTION ST_geometry(text) AS IMPLICIT;
CREATE CAST (geometry AS text) WITH FUNCTION ST_text(geometry) AS IMPLICIT;
CREATE CAST (chip AS geometry) WITH FUNCTION ST_geometry(chip) AS IMPLICIT;
CREATE CAST (bytea AS geometry) WITH FUNCTION ST_geometry(bytea) AS IMPLICIT;
CREATE CAST (geometry AS bytea) WITH FUNCTION ST_bytea(geometry) AS IMPLICIT;
-- CREATE CAST (bool AS text) WITH FUNCTION ST_text(bool) AS IMPLICIT;


---------------------------------------------------------------
-- Algorithms
---------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Simplify(geometry, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_simplify2d'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Simplify(geometry, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_simplify2d'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- SnapToGrid(input, xoff, yoff, xsize, ysize)
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SnapToGrid(geometry, float8, float8, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_snaptogrid'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SnapToGrid(geometry, float8, float8, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_snaptogrid'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- SnapToGrid(input, xsize, ysize) # offsets=0
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SnapToGrid(geometry, float8, float8)
   RETURNS geometry
   AS 'SELECT SnapToGrid($1, 0, 0, $2, $3)'
   LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SnapToGrid(geometry, float8, float8)
   RETURNS geometry
   AS 'SELECT SnapToGrid($1, 0, 0, $2, $3)'
   LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- SnapToGrid(input, size) # xsize=ysize=size, offsets=0
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SnapToGrid(geometry, float8)
   RETURNS geometry
   AS 'SELECT SnapToGrid($1, 0, 0, $2, $2)'
   LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SnapToGrid(geometry, float8)
   RETURNS geometry
   AS 'SELECT SnapToGrid($1, 0, 0, $2, $2)'
   LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- SnapToGrid(input, point_offsets, xsize, ysize, zsize, msize)
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SnapToGrid(geometry, geometry, float8, float8, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_snaptogrid_pointoff'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SnapToGrid(geometry, geometry, float8, float8, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_snaptogrid_pointoff'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Segmentize(geometry, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_segmentize2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Segmentize(geometry, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_segmentize2d'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

---------------------------------------------------------------
-- LRS
---------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION line_interpolate_point(geometry, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_interpolate_point'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_line_interpolate_point(geometry, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_interpolate_point'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION line_substring(geometry, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_substring'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_line_substring(geometry, float8, float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_substring'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION line_locate_point(geometry, geometry)
   RETURNS float8
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_locate_point'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_line_locate_point(geometry, geometry)
   RETURNS float8
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_locate_point'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION locate_between_measures(geometry, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_locate_between_m'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_locate_between_measures(geometry, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_locate_between_m'
	LANGUAGE 'C' IMMUTABLE STRICT;

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION locate_along_measure(geometry, float8)
	RETURNS geometry
	AS 'SELECT locate_between_measures($1, $2, $2)'
	LANGUAGE 'sql' IMMUTABLE STRICT;

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_locate_along_measure(geometry, float8)
	RETURNS geometry
	AS 'SELECT locate_between_measures($1, $2, $2)'
	LANGUAGE 'sql' IMMUTABLE STRICT;

---------------------------------------------------------------
-- GEOS
---------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION intersection(geometry,geometry)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom','intersection'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: intersection(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Intersection(geometry,geometry)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','intersection'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION buffer(geometry,float8)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom','buffer'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: buffer(geometry,float8)
CREATE OR REPLACE FUNCTION ST_Buffer(geometry,float8)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','buffer'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION buffer(geometry,float8,integer)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom','buffer'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_buffer(geometry,float8,integer)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom','buffer'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);
   
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION convexhull(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','convexhull'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: convexhull(geometry)
CREATE OR REPLACE FUNCTION ST_ConvexHull(geometry)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','convexhull'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-- Requires GEOS >= 3.0.0
-- Availability: 1.3.3
CREATE OR REPLACE FUNCTION ST_SimplifyPreserveTopology(geometry, float8)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','topologypreservesimplify'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION difference(geometry,geometry)
	RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','difference'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: difference(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Difference(geometry,geometry)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','difference'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION boundary(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','boundary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: boundary(geometry)
CREATE OR REPLACE FUNCTION ST_Boundary(geometry)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','boundary'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION symdifference(geometry,geometry)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','symdifference'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: symdifference(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_SymDifference(geometry,geometry)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','symdifference'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION symmetricdifference(geometry,geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','symdifference'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_symmetricdifference(geometry,geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','symdifference'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomUnion(geometry,geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','geomunion'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: GeomUnion(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Union(geometry,geometry)
    RETURNS geometry
    AS '%{INSTALL_PATH}%/lib/liblwgeom','geomunion'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE AGGREGATE MemGeomUnion (
	basetype = geometry,
	sfunc = geomunion,
	stype = geometry
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_MemUnion (
	basetype = geometry,
	sfunc = ST_union,
	stype = geometry
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION unite_garray (geometry[])
	RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable); 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_unite_garray (geometry[])
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','unite_garray'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable); 

-- Deprecation in 1.2.3
CREATE AGGREGATE GeomUnion (
	sfunc = geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_unite_garray
	);

-- Availability: 1.2.2
CREATE AGGREGATE ST_Union (
	sfunc = ST_geom_accum,
	basetype = geometry,
	stype = geometry[],
	finalfunc = ST_unite_garray
	);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION relate(geometry,geometry)
   RETURNS text
   AS '%{INSTALL_PATH}%/lib/liblwgeom','relate_full'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_relate(geometry,geometry)
   RETURNS text
   AS '%{INSTALL_PATH}%/lib/liblwgeom','relate_full'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION relate(geometry,geometry,text)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom','relate_pattern'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: relate(geometry,geometry,text)
CREATE OR REPLACE FUNCTION ST_Relate(geometry,geometry,text)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','relate_pattern'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION disjoint(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);
   
-- PostGIS equivalent function: disjoint(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Disjoint(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','disjoint'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION touches(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: touches(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Touches(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','touches'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Touches(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Touches($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_DWithin(geometry, geometry, float8)
    RETURNS boolean
    AS 'SELECT $1 && ST_Expand($2,$3) AND $2 && ST_Expand($1,$3) AND ST_Distance($1, $2) < $3'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION intersects(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: intersects(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Intersects(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','intersects'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Intersects(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Intersects($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION crosses(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: crosses(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Crosses(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','crosses'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Crosses(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Crosses($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION within(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: within(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Within(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','within'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Within(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Within($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Contains(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: contains(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Contains(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','contains'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Contains(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Contains($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);


-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION _ST_CoveredBy(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'coveredby'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_CoveredBy(geometry,geometry)
   RETURNS boolean
   AS 'SELECT $1 && $2 AND _ST_CoveredBy($1,$2)'
   LANGUAGE 'SQL' IMMUTABLE; -- WITH(iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION _ST_Covers(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'covers'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Covers(geometry,geometry)
   RETURNS boolean
   AS 'SELECT $1 && $2 AND _ST_Covers($1,$2)'
   LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);


-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION overlaps(geometry,geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: overlaps(geometry,geometry)
CREATE OR REPLACE FUNCTION _ST_Overlaps(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','overlaps'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
-- Inlines index magic
CREATE OR REPLACE FUNCTION ST_Overlaps(geometry,geometry)
    RETURNS boolean
    AS 'SELECT $1 && $2 AND _ST_Overlaps($1,$2)'
    LANGUAGE 'SQL' IMMUTABLE; -- WITH (iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION IsValid(geometry)
   RETURNS boolean
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'isvalid'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: IsValid(geometry)
-- TODO: change null returns to true
CREATE OR REPLACE FUNCTION ST_IsValid(geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom', 'isvalid'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GEOSnoop(geometry)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'GEOSnoop'
   LANGUAGE 'C' VOLATILE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION JTSnoop(geometry)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'JTSnoop'
   LANGUAGE 'C' VOLATILE STRICT; -- WITH (isstrict,iscachable);

-- This is also available w/out GEOS 
CREATE OR REPLACE FUNCTION Centroid(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: Centroid(geometry)
CREATE OR REPLACE FUNCTION ST_Centroid(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'centroid'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION IsRing(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: IsRing(geometry)
CREATE OR REPLACE FUNCTION ST_IsRing(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'isring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointOnSurface(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: PointOnSurface(geometry)
CREATE OR REPLACE FUNCTION ST_PointOnSurface(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'pointonsurface'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION IsSimple(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'issimple'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: IsSimple(geometry)	
CREATE OR REPLACE FUNCTION ST_IsSimple(geometry)
        RETURNS boolean
        AS '%{INSTALL_PATH}%/lib/liblwgeom', 'issimple'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Equals(geometry,geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom','geomequals'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: Equals(geometry,geometry)
CREATE OR REPLACE FUNCTION ST_Equals(geometry,geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom','geomequals'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-----------------------------------------------------------------------
-- Prepared Geometry Predicates
-- requires GEOS 3.1.0-CAPI-1.5.0 or better
-----------------------------------------------------------------------


-----------------------------------------------------------------------
-- SVG OUTPUT
-----------------------------------------------------------------------
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsSVG(geometry,int4,int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsSVG(geometry,int4,int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsSVG(geometry,int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsSVG(geometry,int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsSVG(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsSVG(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','assvg_geometry'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-----------------------------------------------------------------------
-- GML OUTPUT
-----------------------------------------------------------------------
-- _ST_AsGML(version, geom, precision)
CREATE OR REPLACE FUNCTION _ST_AsGML(int4, geometry, int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asGML'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- AsGML(geom, precision) / version=2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsGML(geometry, int4)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML(2, $1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsGML(geometry, int4)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML(2, $1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- AsGML(geom) / precision=15 version=2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsGML(geometry)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML(2, $1, 15)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availabiltiy: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsGML(geometry)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML(2, $1, 15)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_AsGML(version, geom) / precision=15 version=2
-- Availabiltiy: 1.3.2
CREATE OR REPLACE FUNCTION ST_AsGML(int4, geometry)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML($1, $2, 15)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_AsGML(version, geom, precision)
-- Availabiltiy: 1.3.2
CREATE OR REPLACE FUNCTION ST_AsGML(int4, geometry, int4)
	RETURNS TEXT
	AS 'SELECT _ST_AsGML($1, $2, $3)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-----------------------------------------------------------------------
-- KML OUTPUT
-----------------------------------------------------------------------
-- _ST_AsKML(version, geom, precision)
CREATE OR REPLACE FUNCTION _ST_AsKML(int4, geometry, int4)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asKML'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


-- AsKML(geom, precision) / version=2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsKML(geometry, int4)
	RETURNS TEXT
	AS 'SELECT _ST_AsKML(2, transform($1,4326), $2)' 
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsKML(geometry, int4)
	RETURNS TEXT
	AS 'SELECT _ST_AsKML(2, transform($1,4326), $2)' 
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- AsKML(geom) / precision=15 version=2
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsKML(geometry) 
	RETURNS TEXT
	AS 'SELECT _ST_AsKML(2, transform($1,4326), 15)' 
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- AsKML(version, geom, precision)
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsKML(int4, geometry, int4) 
	RETURNS TEXT
	AS 'SELECT _ST_AsKML($1, transform($2,4326), $3)' 
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availabiltiy: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsKML(geometry)
	RETURNS TEXT
	AS 'SELECT _ST_AsKML(2, transform($1,4326), 15)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_AsKML(version, geom) / precision=15 version=2
-- Availabiltiy: 1.3.2
CREATE OR REPLACE FUNCTION ST_AsKML(int4, geometry)
	RETURNS TEXT
	AS 'SELECT _ST_AsKML($1, transform($2,4326), 15)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_AsKML(version, geom, precision)
-- Availabiltiy: 1.3.2
CREATE OR REPLACE FUNCTION ST_AsKML(int4, geometry, int4) 
	RETURNS TEXT
	AS 'SELECT _ST_AsKML($1, transform($2,4326), $3)' 
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);


------------------------------------------------------------------------
-- OGC defined
------------------------------------------------------------------------

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION NumPoints(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_numpoints_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: NumPoints(geometry)
CREATE OR REPLACE FUNCTION ST_NumPoints(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_numpoints_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION NumGeometries(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_numgeometries_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: NumGeometries(geometry)
CREATE OR REPLACE FUNCTION ST_NumGeometries(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_numgeometries_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeometryN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_geometryn_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: GeometryN(geometry)
CREATE OR REPLACE FUNCTION ST_GeometryN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_geometryn_collection'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Dimension(geometry)
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dimension'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: Dimension(geometry)
CREATE OR REPLACE FUNCTION ST_Dimension(geometry)
    RETURNS int4
    AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_dimension'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION ExteriorRing(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_exteriorring_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: ExteriorRing(geometry)
CREATE OR REPLACE FUNCTION ST_ExteriorRing(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_exteriorring_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION NumInteriorRings(geometry)
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_numinteriorrings_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: NumInteriorRings(geometry)
CREATE OR REPLACE FUNCTION ST_NumInteriorRings(geometry)
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_numinteriorrings_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION NumInteriorRing(geometry)
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_numinteriorrings_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_NumInteriorRing(geometry)
	RETURNS integer
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_numinteriorrings_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION InteriorRingN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_interiorringn_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: InteriorRingN(geometry)
CREATE OR REPLACE FUNCTION ST_InteriorRingN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_interiorringn_polygon'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeometryType(geometry)
	RETURNS text
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_getTYPE'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Not quite equivalent to GeometryType
CREATE OR REPLACE FUNCTION ST_GeometryType(geometry)
    RETURNS text
    AS '
    DECLARE
        gtype text := geometrytype($1);
    BEGIN
        IF (gtype IN (''POINT'', ''POINTM'')) THEN
            gtype := ''Point'';
        ELSIF (gtype IN (''LINESTRING'', ''LINESTRINGM'')) THEN
            gtype := ''LineString'';
        ELSIF (gtype IN (''POLYGON'', ''POLYGONM'')) THEN
            gtype := ''Polygon'';
        ELSIF (gtype IN (''MULTIPOINT'', ''MULTIPOINTM'')) THEN
            gtype := ''MultiPoint'';
        ELSIF (gtype IN (''MULTILINESTRING'', ''MULTILINESTRINGM'')) THEN
            gtype := ''MultiLineString'';
        ELSIF (gtype IN (''MULTIPOLYGON'', ''MULTIPOLYGONM'')) THEN
            gtype := ''MultiPolygon'';
        ELSE
            gtype := ''Geometry'';
        END IF;
        RETURN ''ST_'' || gtype;
    END
	'
	LANGUAGE 'plpgsql' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_pointn_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: PointN(geometry,integer)
CREATE OR REPLACE FUNCTION ST_PointN(geometry,integer)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_pointn_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION X(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_x_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: X(geometry)
CREATE OR REPLACE FUNCTION ST_X(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_x_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Y(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_y_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);
        
-- PostGIS equivalent function: Y(geometry)
CREATE OR REPLACE FUNCTION ST_Y(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_y_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION Z(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_z_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: Z(geometry)
CREATE OR REPLACE FUNCTION SE_Z(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_z_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_Z(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_z_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION M(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_m_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_M(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_m_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION StartPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_startpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: StartPoint(geometry))
CREATE OR REPLACE FUNCTION ST_StartPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_startpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION EndPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_endpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: EndPoint(geometry))
CREATE OR REPLACE FUNCTION ST_EndPoint(geometry)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_endpoint_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION IsClosed(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_isclosed_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: IsClosed(geometry)
CREATE OR REPLACE FUNCTION ST_IsClosed(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_isclosed_linestring'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION IsEmpty(geometry)
	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_isempty'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: IsEmpty(geometry)
CREATE OR REPLACE FUNCTION ST_IsEmpty(geometry)
    RETURNS boolean
    AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_isempty'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SRID(geometry) 
	RETURNS int4
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_getSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: getSRID(geometry)
CREATE OR REPLACE FUNCTION ST_SRID(geometry) 
    RETURNS int4
    AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_getSRID'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION SetSRID(geometry,int4) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_setSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);	

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_SetSRID(geometry,int4) 
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_setSRID'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);	
	
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsBinary(geometry)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asBinary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: AsBinary(geometry)
CREATE OR REPLACE FUNCTION ST_AsBinary(geometry)
    RETURNS bytea
    AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asBinary'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsBinary(geometry,text)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asBinary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_AsBinary(geometry,text)
	RETURNS bytea
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asBinary'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION AsText(geometry)
	RETURNS TEXT
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asText'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: AsText(geometry)
CREATE OR REPLACE FUNCTION ST_AsText(geometry)
    RETURNS TEXT
    AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_asText'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeometryFromText(text)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeometryFromText(text)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeometryFromText(text, int4)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeometryFromText(text, int4)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_text'
        LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromText(text)
	RETURNS geometry AS 'SELECT geometryfromtext($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomFromText(text)
	RETURNS geometry AS 'SELECT geometryfromtext($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromText(text, int4)
	RETURNS geometry AS 'SELECT geometryfromtext($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: GeometryFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_GeomFromText(text, int4)
	RETURNS geometry AS 'SELECT geometryfromtext($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''POINT''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PointFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''POINT''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''POINT''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: PointFromText(text, int4)
-- TODO: improve this ... by not duplicating constructor time.
CREATE OR REPLACE FUNCTION ST_PointFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''POINT''
	THEN GeomFromText($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''LINESTRING''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LineFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''LINESTRING''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''LINESTRING''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: LineFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_LineFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''LINESTRING''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineStringFromText(text)
	RETURNS geometry
	AS 'SELECT LineFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineStringFromText(text, int4)
	RETURNS geometry
	AS 'SELECT LineFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolyFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''POLYGON''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolyFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''POLYGON''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolyFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''POLYGON''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);
        
-- PostGIS equivalent function: PolyFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_PolyFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''POLYGON''
	THEN GeomFromText($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolygonFromText(text, int4)
	RETURNS geometry
	AS 'SELECT PolyFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolygonFromText(text, int4)
	RETURNS geometry
	AS 'SELECT PolyFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolygonFromText(text)
	RETURNS geometry
	AS 'SELECT PolyFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolygonFromText(text)
	RETURNS geometry
	AS 'SELECT PolyFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MLineFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MLineFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_MLineFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MLineFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTILINESTRING''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MLineFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTILINESTRING''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiLineStringFromText(text)
	RETURNS geometry
	AS 'SELECT MLineFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiLineStringFromText(text)
	RETURNS geometry
	AS 'SELECT MLineFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiLineStringFromText(text, int4)
	RETURNS geometry
	AS 'SELECT MLineFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiLineStringFromText(text, int4)
	RETURNS geometry
	AS 'SELECT MLineFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPointFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1,$2)) = ''MULTIPOINT''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MPointFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_MPointFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''MULTIPOINT''
	THEN GeomFromText($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPointFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTIPOINT''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MPointFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTIPOINT''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPointFromText(text, int4)
	RETURNS geometry
	AS 'SELECT MPointFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPointFromText(text)
	RETURNS geometry
	AS 'SELECT MPointFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPointFromText(text)
	RETURNS geometry
	AS 'SELECT MPointFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPointFromText(text)
	RETURNS geometry
	AS 'SELECT MPointFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPointFromText(text)
	RETURNS geometry
	AS 'SELECT MPointFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPolyFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MPolyFromText(text, int4)
CREATE OR REPLACE FUNCTION ST_MPolyFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPolyFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTIPOLYGON''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

--Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MPolyFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = ''MULTIPOLYGON''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPolygonFromText(text, int4)
	RETURNS geometry
	AS 'SELECT MPolyFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPolygonFromText(text, int4)
	RETURNS geometry
	AS 'SELECT MPolyFromText($1, $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPolygonFromText(text)
	RETURNS geometry
	AS 'SELECT MPolyFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPolygonFromText(text)
	RETURNS geometry
	AS 'SELECT MPolyFromText($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomCollFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomCollFromText(text, int4)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromText($1,$2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomCollFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomCollFromText(text)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromText($1)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromText($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromWKB(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_WKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomFromWKB(bytea)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_from_WKB'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomFromWKB(bytea, int)
	RETURNS geometry
	AS 'SELECT setSRID(GeomFromWKB($1), $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: GeomFromWKB(bytea, int)
CREATE OR REPLACE FUNCTION ST_GeomFromWKB(bytea, int)
	RETURNS geometry
	AS 'SELECT setSRID(GeomFromWKB($1), $2)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''POINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: PointFromWKB(bytea, int)
CREATE OR REPLACE FUNCTION ST_PointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''POINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''LINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: LineFromWKB(text, int)
CREATE OR REPLACE FUNCTION ST_LineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''LINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''LINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''LINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LinestringFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''LINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LinestringFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''LINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION LinestringFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''LINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_LinestringFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''LINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''POLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: PolyFromWKB(text, int)
CREATE OR REPLACE FUNCTION ST_PolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''POLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolygonFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = ''POLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolygonFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = ''POLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION PolygonFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_PolygonFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''POLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = ''MULTIPOINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MPointFromWKB(text, int)
CREATE OR REPLACE FUNCTION ST_MPointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTIPOINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MPointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = ''MULTIPOINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPointFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = ''MULTIPOINT''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPointFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOINT''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiLineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION MultiLineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiLineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiLineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MLineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MLineFromWKB(text, int)
CREATE OR REPLACE FUNCTION ST_MLineFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MLineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MLineFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTILINESTRING''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: MPolyFromWKB(text, int)
CREATE OR REPLACE FUNCTION ST_MPolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MPolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MPolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPolyFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION MultiPolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_MultiPolyFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = ''MULTIPOLYGON''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomCollFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1, $2)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomCollFromWKB(bytea, int)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1, $2)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION GeomCollFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_GeomCollFromWKB(bytea)
	RETURNS geometry
	AS '
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1)) = ''GEOMETRYCOLLECTION''
	THEN GeomFromWKB($1)
	ELSE NULL END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

--
-- SFSQL 1.1
--
-- BdPolyFromText(multiLineStringTaggedText String, SRID Integer): Polygon
--
--  Construct a Polygon given an arbitrary
--  collection of closed linestrings as a
--  MultiLineString text representation.
--
-- This is a PLPGSQL function rather then an SQL function
-- To avoid double call of BuildArea (one to get GeometryType
-- and another to actual return, in a CASE WHEN construct).
-- Also, we profit from plpgsql to RAISE exceptions.
--
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION BdPolyFromText(text, integer)
RETURNS geometry
AS '
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION ''Input is not a MultiLinestring'';
	END IF;

	geom := BuildArea(mline);

	IF GeometryType(geom) != ''POLYGON''
	THEN
		RAISE EXCEPTION ''Input returns more then a single polygon, try using BdMPolyFromText instead'';
	END IF;

	RETURN geom;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_BdPolyFromText(text, integer)
RETURNS geometry
AS '
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION ''Input is not a MultiLinestring'';
	END IF;

	geom := BuildArea(mline);

	IF GeometryType(geom) != ''POLYGON''
	THEN
		RAISE EXCEPTION ''Input returns more then a single polygon, try using BdMPolyFromText instead'';
	END IF;

	RETURN geom;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; 

--
-- SFSQL 1.1
--
-- BdMPolyFromText(multiLineStringTaggedText String, SRID Integer): MultiPolygon
--
--  Construct a MultiPolygon given an arbitrary
--  collection of closed linestrings as a
--  MultiLineString text representation.
--
-- This is a PLPGSQL function rather then an SQL function
-- To raise an exception in case of invalid input.
--
-- Deprecation in 1.2.3
CREATE OR REPLACE FUNCTION BdMPolyFromText(text, integer)
RETURNS geometry
AS '
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION ''Input is not a MultiLinestring'';
	END IF;

	geom := multi(BuildArea(mline));

	RETURN geom;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; 

-- Availability: 1.2.2
CREATE OR REPLACE FUNCTION ST_BdMPolyFromText(text, integer)
RETURNS geometry
AS '
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION ''Input is not a MultiLinestring'';
	END IF;

	geom := multi(BuildArea(mline));

	RETURN geom;
END;
'
LANGUAGE 'plpgsql' IMMUTABLE STRICT; 


-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- 
-- $Id: long_xact.sql.in 2671 2007-07-26 18:55:37Z mleslie $
--
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.refractions.net
-- Copyright 2001-2003 Refractions Research Inc.
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -





-----------------------------------------------------------------------
-- LONG TERM LOCKING
-----------------------------------------------------------------------

-- UnlockRows(authid)
-- removes all locks held by the given auth
-- returns the number of locks released
CREATE OR REPLACE FUNCTION UnlockRows(text)
	RETURNS int
	AS '
DECLARE
	ret int;
BEGIN

	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION ''Long transaction support disabled, use EnableLongTransaction() to enable.'';
	END IF;

	EXECUTE ''DELETE FROM authorization_table where authid = '' ||
		quote_literal($1);

	GET DIAGNOSTICS ret = ROW_COUNT;

	RETURN ret;
END;
'
LANGUAGE 'plpgsql' VOLATILE STRICT;

-- LockRow([schema], table, rowid, auth, [expires]) 
-- Returns 1 if successfully obtained the lock, 0 otherwise
CREATE OR REPLACE FUNCTION LockRow(text, text, text, text, timestamp)
	RETURNS int
	AS '
DECLARE
	myschema alias for $1;
	mytable alias for $2;
	myrid   alias for $3;
	authid alias for $4;
	expires alias for $5;
	ret int;
	mytoid oid;
	myrec RECORD;
	
BEGIN

	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION ''Long transaction support disabled, use EnableLongTransaction() to enable.'';
	END IF;

	EXECUTE ''DELETE FROM authorization_table WHERE expires < now()''; 


	SELECT c.oid INTO mytoid FROM pg_class c, pg_namespace n
		WHERE c.relname = mytable
		AND c.relnamespace = n.oid
		AND n.nspname = myschema;





	-- RAISE NOTICE ''toid: %'', mytoid;

	FOR myrec IN SELECT * FROM authorization_table WHERE 
		toid = mytoid AND rid = myrid
	LOOP
		IF myrec.authid != authid THEN
			RETURN 0;
		ELSE
			RETURN 1;
		END IF;
	END LOOP;

	EXECUTE ''INSERT INTO authorization_table VALUES (''||
		quote_literal(mytoid::text)||'',''||quote_literal(myrid)||
		'',''||quote_literal(expires::text)||
		'',''||quote_literal(authid) ||'')'';

	GET DIAGNOSTICS ret = ROW_COUNT;

	RETURN ret;
END;'
LANGUAGE 'plpgsql' VOLATILE STRICT;

-- LockRow(schema, table, rid, authid);
CREATE OR REPLACE FUNCTION LockRow(text, text, text, text)
	RETURNS int
	AS
'SELECT LockRow($1, $2, $3, $4, now()::timestamp+''1:00'');'
	LANGUAGE 'sql' VOLATILE STRICT;

-- LockRow(table, rid, authid);
CREATE OR REPLACE FUNCTION LockRow(text, text, text)
	RETURNS int
	AS

'SELECT LockRow(current_schema(), $1, $2, $3, now()::timestamp+''1:00'');'



	LANGUAGE 'sql' VOLATILE STRICT;

-- LockRow(schema, table, rid, expires);
CREATE OR REPLACE FUNCTION LockRow(text, text, text, timestamp)
	RETURNS int
	AS

'SELECT LockRow(current_schema(), $1, $2, $3, $4);'



	LANGUAGE 'sql' VOLATILE STRICT;


CREATE OR REPLACE FUNCTION AddAuth(text)
	RETURNS BOOLEAN
	AS '
DECLARE
	lockid alias for $1;
	okay boolean;
	myrec record;
BEGIN
	-- check to see if table exists
	--  if not, CREATE TEMP TABLE mylock (transid xid, lockcode text)
	okay := ''f'';
	FOR myrec IN SELECT * FROM pg_class WHERE relname = ''temp_lock_have_table'' LOOP
		okay := ''t'';
	END LOOP; 
	IF (okay <> ''t'') THEN 
		CREATE TEMP TABLE temp_lock_have_table (transid xid, lockcode text);
			-- this will only work from pgsql7.4 up
			-- ON COMMIT DELETE ROWS;
	END IF;

	--  INSERT INTO mylock VALUES ( $1)
--	EXECUTE ''INSERT INTO temp_lock_have_table VALUES ( ''||
--		quote_literal(getTransactionID()) || '','' ||
--		quote_literal(lockid) ||'')'';

	INSERT INTO temp_lock_have_table VALUES (getTransactionID(), lockid);

	RETURN true::boolean;
END;
'
LANGUAGE PLPGSQL;
 

-- CheckAuth( <schema>, <table>, <ridcolumn> )
--
-- Returns 0
--
CREATE OR REPLACE FUNCTION CheckAuth(text, text, text)
	RETURNS INT
	AS '
DECLARE

	schema text;

BEGIN
	IF NOT LongTransactionsEnabled() THEN
		RAISE EXCEPTION ''Long transaction support disabled, use EnableLongTransaction() to enable.'';
	END IF;


	if ( $1 != '''' ) THEN
		schema = $1;
	ELSE
		SELECT current_schema() into schema;
	END IF;


	-- TODO: check for an already existing trigger ?

	EXECUTE ''CREATE TRIGGER check_auth BEFORE UPDATE OR DELETE ON '' 

		|| quote_ident(schema) || ''.'' || quote_ident($2)



		||'' FOR EACH ROW EXECUTE PROCEDURE CheckAuthTrigger(''
		|| quote_literal($3) || '')'';

	RETURN 0;
END;
'
LANGUAGE 'plpgsql';

-- CheckAuth(<table>, <ridcolumn>)
CREATE OR REPLACE FUNCTION CheckAuth(text, text)
	RETURNS INT
	AS
	'SELECT CheckAuth('''', $1, $2)'
	LANGUAGE 'SQL';

CREATE OR REPLACE FUNCTION CheckAuthTrigger()
	RETURNS trigger AS 
	'%{INSTALL_PATH}%/lib/liblwgeom', 'check_authorization'
	LANGUAGE C;

CREATE OR REPLACE FUNCTION GetTransactionID()
	RETURNS xid AS 
	'%{INSTALL_PATH}%/lib/liblwgeom', 'getTransactionID'
	LANGUAGE C;


--
-- Enable Long transactions support
--
--  Creates the authorization_table if not already existing
--
CREATE OR REPLACE FUNCTION EnableLongTransactions()
	RETURNS TEXT
	AS '
DECLARE
	"query" text;
	exists bool;
	rec RECORD;

BEGIN

	exists = ''f'';
	FOR rec IN SELECT * FROM pg_class WHERE relname = ''authorization_table''
	LOOP
		exists = ''t'';
	END LOOP;

	IF NOT exists
	THEN
		"query" = ''CREATE TABLE authorization_table (
			toid oid, -- table oid
			rid text, -- row id
			expires timestamp,
			authid text
		)'';
		EXECUTE "query";
	END IF;

	exists = ''f'';
	FOR rec IN SELECT * FROM pg_class WHERE relname = ''authorized_tables''
	LOOP
		exists = ''t'';
	END LOOP;

	IF NOT exists THEN
		"query" = ''CREATE VIEW authorized_tables AS '' ||
			''SELECT '' ||

			''n.nspname as schema, '' ||

			''c.relname as table, trim('' ||
			quote_literal(chr(92) || ''000'') ||
			'' from t.tgargs) as id_column '' ||
			''FROM pg_trigger t, pg_class c, pg_proc p '' ||

			'', pg_namespace n '' ||

			''WHERE p.proname = '' || quote_literal(''checkauthtrigger'') ||

			'' AND c.relnamespace = n.oid'' ||

			'' AND t.tgfoid = p.oid and t.tgrelid = c.oid'';
		EXECUTE "query";
	END IF;

	RETURN ''Long transactions support enabled'';
END;
'
LANGUAGE 'plpgsql';

--
-- Check if Long transactions support is enabled
--
CREATE OR REPLACE FUNCTION LongTransactionsEnabled()
	RETURNS bool
AS '
DECLARE
	rec RECORD;
BEGIN
	FOR rec IN SELECT oid FROM pg_class WHERE relname = ''authorized_tables''
	LOOP
		return ''t'';
	END LOOP;
	return ''f'';
END;
'
LANGUAGE 'plpgsql';

--
-- Disable Long transactions support
--
--  (1) Drop any long_xact trigger 
--  (2) Drop the authorization_table
--  (3) KEEP the authorized_tables view
--
CREATE OR REPLACE FUNCTION DisableLongTransactions()
	RETURNS TEXT
	AS '
DECLARE
	rec RECORD;

BEGIN

	--
	-- Drop all triggers applied by CheckAuth()
	--
	FOR rec IN
		SELECT c.relname, t.tgname, t.tgargs FROM pg_trigger t, pg_class c, pg_proc p
		WHERE p.proname = ''checkauthtrigger'' and t.tgfoid = p.oid and t.tgrelid = c.oid
	LOOP
		EXECUTE ''DROP TRIGGER '' || quote_ident(rec.tgname) ||
			'' ON '' || quote_ident(rec.relname);
	END LOOP;

	--
	-- Drop the authorization_table table
	--
	FOR rec IN SELECT * FROM pg_class WHERE relname = ''authorization_table'' LOOP
		DROP TABLE authorization_table;
	END LOOP;

	--
	-- Drop the authorized_tables view
	--
	FOR rec IN SELECT * FROM pg_class WHERE relname = ''authorized_tables'' LOOP
		DROP VIEW authorized_tables;
	END LOOP;

	RETURN ''Long transactions support disabled'';
END;
'
LANGUAGE 'plpgsql';

---------------------------------------------------------------
-- END
---------------------------------------------------------------


-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- 
-- $Id: sqlmm.sql.in 2406 2006-11-02 13:56:52Z kneufeld $
--
-- PostGIS - Spatial Types for PostgreSQL
-- http://postgis.refractions.net
-- Copyright 2001-2003 Refractions Research Inc.
--
-- This is free software; you can redistribute and/or modify it under
-- the terms of the GNU General Public Licence. See the COPYING file.
--  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-- This file defines a subset of SQL/MM functions (that is, only those
-- currently defined by ESRI's ArcSDE). Since these functions already exist
-- in PostGIS (for the most part), these functions simply expose the current
-- functions. Although mostly complying with SQL/MM standards, these prototypes
-- follow ESRI's ArcSDE SQL Specifications and not SQL/MM standards where 
-- disparities exist.
--
-- Specification Disparity Notes:
--   * ST_OrderingEquals(geometry, geometry) is implemented as per
--     ESRI's ArcSDE SQL specifications, not SQL/MM specifications.
--     (http://edndoc.esri.com/arcsde/9.1/sql_api/sqlapi3.htm#ST_OrderingEquals)
--   * Geometry constructors default to an SRID of -1, not 0 as per SQL/MM specs.
--   * Boolean return type methods (ie. ST_IsValid, ST_IsEmpty, ...)
--      * SQL/MM           : RETURNS 1 if TRUE, 0 if (FALSE, NULL)
--      * ESRI in Informix : RETURNS 1 if (TRUE, NULL), 0 if FALSE
--      * ESRI in DB2      : RETURNS 1 if TRUE, 0 if FALSE, NULL if NULL 
--      * PostGIS          : RETURNS 1 if TRUE, 0 if FALSE, NULL if NULL 
--
-- TODO: Implement ESRI's Shape constructors
--   * SE_AsShape(geometry)
--   * SE_ShapeToSQL
--   * SE_GeomFromShape
--   * SE_PointFromShape
--   * SE_LineFromShape
--   * SE_PolyFromShape
--   * SE_MPointFromShape
--   * SE_MLineFromShape
--   * SE_MPolyFromShape
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for constructing an ST_Geometry
--     value given its WTK representation
-- (http://edndoc.esri.com/arcsde/9.1/general_topics/storing_geo_in_rdbms.html)
-------------------------------------------------------------------------------

-- PostGIS equivalent function: GeometryFromText(text)
-- Note: Defaults to an SRID=-1, not 0 as per SQL/MM specs.
CREATE OR REPLACE FUNCTION ST_WKTToSQL(text)
	RETURNS geometry AS 'SELECT geometryfromtext($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_GeomFromText(text, int4) - already defined
-- ST_PointFromText(text, int4) - already defined
-- ST_LineFromText(text, int4) - already defined
-- ST_PolyFromText(text, int4) - already defined
-- ST_MPointFromText(text, int4) - already defined
-- ST_MLineFromText(text, int4) - already defined
-- ST_MPolyFromText(text, int4) - already defined

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for constructing an ST_Geometry
--     value given its WKB representation
-------------------------------------------------------------------------------

-- PostGIS equivalent function: GeomFromWKB(bytea))
-- Note: Defaults to an SRID=-1, not 0 as per SQL/MM specs.
CREATE OR REPLACE FUNCTION ST_WKBToSQL(bytea)
	RETURNS geometry
	AS 'SELECT GeomFromWKB($1)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_GeomFromWKB(bytea, int) - already defined
-- ST_PointFromWKB(bytea, int) - already defined
-- ST_LineFromWKB(bytea, int) - already defined
-- ST_PolyFromWKB(bytea, int) - already defined
-- ST_MPointFromWKB(bytea, int) - already defined
-- ST_MLineFromWKB(bytea, int) - already defined
-- ST_MPolyFromWKB(bytea, int) - already defined

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for constructing an ST_Geometry
--     value given an ESRI Shape representation
-------------------------------------------------------------------------------

-- TODO: SE_ShapeToSQL
-- TODO: SE_GeomFromShape
-- TODO: SE_PointFromShape
-- TODO: SE_LineFromShape
-- TODO: SE_PolyFromShape
-- TODO: SE_MPointFromShape
-- TODO: SE_MLineFromShape
-- TODO: SE_MPolyFromShape

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for obtaining the WKT representation
--     of an ST_Geometry
-------------------------------------------------------------------------------

-- ST_AsText(geometry) - already defined

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for obtaining the WKB representation
--     of an ST_Geometry
-------------------------------------------------------------------------------

-- ST_AsBinary(geometry) - already defined

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for obtaining the ESRI Shape 
-- representation of an ST_Geometry
-------------------------------------------------------------------------------

-- TODO: SE_AsShape(geometry)
--CREATE OR REPLACE FUNCTION SE_AsShape(geometry)
--    RETURNS bytea
--    AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_AsShape'
--    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_Geometry
-------------------------------------------------------------------------------

-- PostGIS equivalent function: ndims(geometry)
CREATE OR REPLACE FUNCTION ST_CoordDim(geometry)
    RETURNS smallint
    AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_ndims'
    LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- ST_Dimension(geometry) - already defined.
-- ST_GeometryType(geometry) - already defined.
-- ST_SRID(geometry) - already defined.
-- ST_IsEmpty(geometry) - already defined.
-- ST_IsSimple(geometry) - already defined.
-- ST_IsValid(geometry) - already defined.
-- ST_Boundary(geometry) - already defined.
-- ST_Envelope(geometry) - already defined.
-- ST_Transform(geometry) - already defined.
-- ST_AsText(geometry) - already defined.
-- ST_AsBinary(geometry) - already defined.
-- SE_AsShape(geometry) - already defined.
-- ST_X(geometry) - already defined.
-- ST_Y(geometry) - already defined.

-- PostGIS equivalent function: ~= 
CREATE OR REPLACE FUNCTION ST_OrderingEquals(geometry, geometry)
    RETURNS boolean
    AS '
    SELECT $1 && $2 AND $1 ~= $2
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: zmflag(geometry)
CREATE OR REPLACE FUNCTION SE_Is3D(geometry)
    RETURNS boolean
    AS '
    SELECT CASE ST_zmflag($1)
               WHEN 0 THEN false
               WHEN 1 THEN false
               WHEN 2 THEN true
               WHEN 3 THEN true
               ELSE false
           END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- PostGIS equivalent function: zmflag(geometry)
CREATE OR REPLACE FUNCTION SE_IsMeasured(geometry)
    RETURNS boolean
    AS '
    SELECT CASE ST_zmflag($1)
               WHEN 0 THEN false
               WHEN 1 THEN true
               WHEN 2 THEN false
               WHEN 3 THEN true
               ELSE false
           END
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);





-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_Point
-------------------------------------------------------------------------------

-- PostGIS equivalent function: makePoint(float8,float8)
CREATE OR REPLACE FUNCTION ST_Point(float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_makepoint'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (iscachable,isstrict);

-- PostGIS equivalent function: Z(geometry)
CREATE OR REPLACE FUNCTION SE_Z(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_z_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-- PostGIS equivalent function: M(geometry)
CREATE OR REPLACE FUNCTION SE_M(geometry)
	RETURNS float8
	AS '%{INSTALL_PATH}%/lib/liblwgeom','LWGEOM_m_point'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_Curve
-------------------------------------------------------------------------------

-- ST_StartPoint(geometry) - already defined.
-- ST_EndPoint(geometry) - already defined.
-- ST_IsClosed(geometry) - already defined.
-- ST_IsRing(geometry) - already defined.
-- ST_Length(geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_LineString
-------------------------------------------------------------------------------

-- ST_NumPoints(geometry) - already defined.
-- ST_PointN(geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_Surface
-------------------------------------------------------------------------------

-- ST_Centroid(geometry) - already defined.
-- ST_PointOnSurface(geometry) - already defined.
-- ST_Area(geometry) - already defined.
-- ST_Perimeter(geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_Polygon
-------------------------------------------------------------------------------

-- PostGIS equivalent function: MakePolygon(geometry)
CREATE OR REPLACE FUNCTION ST_Polygon(geometry, int)
	RETURNS geometry
	AS '
	SELECT setSRID(makepolygon($1), $2)
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-- ST_ExteriorRing(geometry) - already defined.
-- ST_NumInteriorRing(geometry) - already defined.
-- ST_InteriorRingN(geometry, integer) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_GeomCollection
-------------------------------------------------------------------------------

-- ST_NumGeometries(geometry) - already defined.
-- ST_GeometryN(geometry, integer) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_MultiCurve
-------------------------------------------------------------------------------

-- ST_IsClosed(geometry) - already defined.
-- ST_Length(geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions on type ST_MultiSurface
-------------------------------------------------------------------------------

-- ST_Centroid(geometry) - already defined.
-- ST_PointOnSurface(geometry) - already defined.
-- ST_Area(geometry) - already defined.
-- ST_Perimeter(geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions that test spatial relationships
-------------------------------------------------------------------------------

-- ST_Equals(geometry, geometry) - already defined.
-- ST_Disjoint(geometry, geometry) - already defined.
-- ST_Touches(geometry, geometry) - already defined.
-- ST_Within(geometry, geometry) - already defined.
-- ST_Overlaps(geometry, geometry) - already defined.
-- ST_Crosses(geometry, geometry) - already defined.
-- ST_Intersects(geometry, geometry) - already defined.
-- ST_Contains(geometry, geometry) - already defined.
-- ST_Relate(geometry, geometry, text) - already defined.

-- PostGIS equivalent function: none
CREATE OR REPLACE FUNCTION SE_EnvelopesIntersect(geometry,geometry)
    RETURNS boolean
	AS '
	SELECT $1 && $2
	'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions for distance relationships
-------------------------------------------------------------------------------

-- ST_Distance(geometry, geometry) - already defined.

-------------------------------------------------------------------------------
-- SQL/MM (ArcSDE subset) - SQL Functions that implement spatial operators
-------------------------------------------------------------------------------

-- ST_Intersection(geometry, geometry) - already defined.
-- ST_Difference(geometry, geometry) - already defined.
-- ST_Union(geometry, geometry) - already defined.
-- ST_SymDifference(geometry, geometry) - already defined.
-- ST_Buffer(geometry, float8) - already defined.
-- ST_ConvexHull(geometry) already defined.

-- PostGIS equivalent function: locate_along_measure(geometry, float8)
CREATE OR REPLACE FUNCTION SE_LocateAlong(geometry, float8)
	RETURNS geometry
	AS 'SELECT locate_between_measures($1, $2, $2)'
	LANGUAGE 'sql' IMMUTABLE STRICT;

-- PostGIS equivalent function: locate_between_measures(geometry, float8, float8)
CREATE OR REPLACE FUNCTION SE_LocateBetween(geometry, float8, float8)
	RETURNS geometry
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_locate_between_m'
	LANGUAGE 'C' IMMUTABLE STRICT;



-------------------------------------------------------------------------------
-- END
-------------------------------------------------------------------------------


---------------------------------------------------------------
-- SQL-MM
---------------------------------------------------------------

--
-- SQL-MM
--
-- ST_CurveToLine(Geometry geometry, SegmentsPerQuarter integer)
--
-- Converts a given geometry to a linear geometry.  Each curveed
-- geometry or segment is converted into a linear approximation using
-- the given number of segments per quarter circle.
CREATE OR REPLACE FUNCTION ST_CurveToLine(geometry, integer)
   RETURNS geometry
   AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_curve_segmentize'
   LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);
--
-- SQL-MM
--
-- ST_CurveToLine(Geometry geometry, SegmentsPerQuarter integer)
--
-- Converts a given geometry to a linear geometry.  Each curveed
-- geometry or segment is converted into a linear approximation using
-- the default value of 32 segments per quarter circle
CREATE OR REPLACE FUNCTION ST_CurveToLine(geometry)
	RETURNS geometry AS 'SELECT ST_CurveToLine($1, 32)'
	LANGUAGE 'SQL' IMMUTABLE STRICT; -- WITH (isstrict,iscachable);

CREATE OR REPLACE FUNCTION ST_HasArc(geometry)
 	RETURNS boolean
	AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_has_arc'
	LANGUAGE 'C' IMMUTABLE STRICT; -- WITH (isstrict);

CREATE OR REPLACE FUNCTION ST_LineToCurve(geometry)
        RETURNS geometry
        AS '%{INSTALL_PATH}%/lib/liblwgeom', 'LWGEOM_line_desegmentize'
        LANGUAGE 'C' IMMUTABLE STRICT; 
---------------------------------------------------------------
-- END
---------------------------------------------------------------

COMMIT;

