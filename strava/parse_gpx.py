import os
import pandas as pd
import gpxpy

def parseGPX(file):
    pointlist = []
    with open(file, 'r') as gpxfile:
        if "Run" in file:
            activity = "Run"
        elif "Ride" in file:
            activity = "Ride"
        elif "Hike" in file:
            activity = "Hike"
        else:
            activity = "NA"
        gpx = gpxpy.parse(gpxfile)
        for track in gpx.tracks:
            for segment in track.segments:
                for point in segment.points:
                    dict = {'Timestamp' : point.time,
                            'Latitude' : point.latitude,
                            'Longitude' : point.longitude,
                            'Elevation' : point.elevation,
                            'Activity' : activity
                            }
                    pointlist.append(dict)
    return pointlist
	
gpx_dir = r'D:\strava\strava_rides'
files = os.listdir(gpx_dir)
os.chdir(gpx_dir)

df = pd.concat([pd.DataFrame(parseGPX(file)) for file in files], keys=files)
df.reset_index(level=0, inplace=True)
df.rename(columns={'level_0':'File'}, inplace=True)
df.head()

os.chdir(r'D:\strava')
df.to_csv("strava_rides.csv")