#!/bin/bash

psql -h dx1 -U awips -d metadata -n awips -f updateActiveTable.sql
