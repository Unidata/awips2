package gov.noaa.nws.ncep.viz.gempak.grid.mapper;

import com.raytheon.uf.common.dataplugin.grid.mapping.DatasetIdMapper;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapper;
import com.raytheon.uf.common.parameter.mapping.ParameterMapper;

public class GridMapper {
	public GridMapper () {
	}

	public static void GridMapperInit () {
		initlize ();
	}
	
	private static void initlize () {
		DatasetIdMapper.getInstance();
		LevelMapper.getInstance();
		ParameterMapper.getInstance();
	}
}
