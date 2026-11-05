# Databricks notebook cell – TN-03 RTFM (Model 2) extract, 1–14 June 2025
# Link to PRISME: ifps_id (Databricks) = IFPS_PLAN_ID (FLX.FLIGHT), unique for all 950 sample flights.
# Keys file from R: data/tbo/rtfm-keys-20250601-20250614.parquet (upload to Databricks, e.g. a volume or DBFS).
from pyspark.sql import functions as F, Window as W

pairs = ["EGLL-EDDM", "EDDM-EGLL", "EHAM-LEMD", "LEMD-EHAM", "LTFM-EDDF", "EDDF-LTFM"]

base = (df_flights
        .filter((F.col("lobt") >= "2025-05-31") & (F.col("lobt") < "2025-06-16"))
        .withColumn("pair", F.concat_ws("-", "departure_aerodrome_icao_id", "arrival_aerodrome_icao_id"))
        .filter(F.col("pair").isin(pairs))
        .filter(F.col("rtfm_all_ft_pt_nr_inst") > 0))

# One version per flight: the latest (archive state after operation)
w = W.partitionBy("ifps_id").orderBy(F.col("version_date").desc(), F.col("flight_version_nr").desc_nulls_last())
latest = base.withColumn("rk", F.row_number().over(w)).filter("rk = 1").drop("rk")

print(base.select("ifps_id").distinct().count(), "flights;",
      base.count(), "versions;", latest.count(), "latest rows")

rtfm = (latest
        .select("ifps_id", "flight_id", "version_date", "flight_version_nr", "aircraft_id",
                "departure_aerodrome_icao_id", "arrival_aerodrome_icao_id", "lobt", "cobt", "atfm_delay",
                "most_penalizing_regulation_id", "rerouting_why",
                F.posexplode("rtfm_all_ft_pt_list").alias("seq", "pt"))
        .select("*", "pt.*").drop("pt"))

print(rtfm.count(), "points")
display(rtfm.limit(20))
# rtfm.toPandas().to_parquet("/dbfs/FileStore/tbo/rtfm-sample-20250601-20250614.parquet")
