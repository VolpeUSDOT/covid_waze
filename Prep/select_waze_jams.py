
# ==================================================================================================
#
# Name:        select_waze_jams.py
#
# Purpose:     Select all jams and geometeries during a defined time and area
#
# Author:      Michelle Gilmore
#
# Created:     05/07/2020
#
# Version:     1.0
#
# ==================================================================================================
# IMPORT REQUIRED MODULES
# ==================================================================================================

import os 
import sys
import pandas as pd 
import geopandas as gpd 
import datetime 
from sqlalchemy import create_engine


# ==================================================================================================
# CONFIG SECTION
# ==================================================================================================

## MMG ##
database_engine = '' ## see example below for parameters needed 


# ==================================================================================================
# GLOBAL VARIABLES
# ==================================================================================================

OUTPUT_DIR = r'' 

OUTPUT_SF = 'waze_jams'

STATE = 'MA'

START_DATE = '2020-01-01'

END_DATE = '2020-03-31'


# ==================================================================================================
# MAIN  
# ==================================================================================================

start_time = datetime.datetime.now()
print("\nSelecting Waze Jam files in {} during {} and {}.".format(STATE, START_DATE, END_DATE))
print("Start time : {}".format(start_time))


engine = create_engine(database_engine) 


get_data_from_redsift = '''
SELECT 
	a.*, 
	geography::STGeomFromText('LINESTRING(' + 
		STRING_AGG(CONCAT_WS(' ', LocationGeog.b.location_y, LocationGeog.b.location_x),
		',') + ')',4326) AS geometry
FROM dw_waze.jam_point_seqence a 
WHERE EXISTS (
	SELECT * 
	FROM dw_waze.jam b 
	WHERE a.jam_uuid = b.jam_uuid
	AND STATE = {y} 
	AND pub_utc_timestamp BETWEEN {y} and {y}
	ORDER BY jam_uuid, sequence_order
);
'''.format(y = STATE, START_DATE, END_DATE)


# execute sql query to load data into pandas df 
print('Querying data from redshift')
jams_df = pd.read_sql_query(get_data_from_redsift, engine) ## see below for example 

# convert pandas dataframe to a geodataframe 
jams_gdf = geopandas.GeoDataFrame(jams_df, geometry='geometry') 

# export geodataframe to shapefile
print('Exporting data to shapefile')
jams_gdf.to_file(os.path.join(OUTPUT_DIR, OUTPUT_SF + '.shp'), driver='ESRI Shapefile')



### engine = create_engine('postgresql://scott:tiger@hredshift_host:5439/mydatabase')


total_run_time = datetime.datetime.now() - start_time
print("Run time (H:M:S) : {}".format(total_run_time))
