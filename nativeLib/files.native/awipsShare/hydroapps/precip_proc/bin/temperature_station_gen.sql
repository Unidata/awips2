-- gather distinct lid,ts combinations from temperature table where extremum='X' 

select lid, ts 
into temp temptable
from temperature
where extremum='X'
group by 1,2
order by lid
;

-- add info from location table for the lid 

select
  l.lid,
  '  TAI'||t.ts||'XZ',
   to_char(l.lat,'99.99'),
   to_char(l.lon,'999.99'),
   case    
     when l.elev >= 0 then to_char(l.elev,'99999')
     else '  -999'
   end,
   '9',
   case
     when l.name ISNULL then 'XXXXXXXXXX'
     else l.name
   end
from temptable t, location l
where t.lid=l.lid
order by l.lid asc;
