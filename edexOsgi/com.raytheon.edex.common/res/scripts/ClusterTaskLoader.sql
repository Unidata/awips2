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
DELETE FROM awips.cluster_task where name = 'MasterLock' and details = 'NewRowLock';

INSERT INTO awips.cluster_task (name, details, extraInfo, running, lastExecution)
   values ('MasterLock', 'NewRowLock', null, false, 0);

DELETE FROM awips.cluster_task where name = 'GfeSmartInit' and details = 'RunCheck';

INSERT INTO awips.cluster_task (name, details, extraInfo, running, lastExecution)
   values ('RunCheck', 'GfeSmartInit', null, false, 0);

DELETE FROM awips.cluster_task where name = 'grib' and details = 'spatialCache';

INSERT INTO awips.cluster_task (name, details, extraInfo, running, lastExecution)
   values ('grib', 'spatialCache', null, false, 0);
