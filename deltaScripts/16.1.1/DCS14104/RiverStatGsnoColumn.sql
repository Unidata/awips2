--resize the column gsno in riverstat table

UPDATE pg_attribute SET atttypmod = 15+4 WHERE attrelid= 'riverstat'::regclass AND attname = 'gsno';
