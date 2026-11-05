"""TN-03 Aireon trial: pull ECTRL Aireon messages for selected flights from Hive.

Reuses the data-fetcher Hive JDBC setup (driver jar + .streamlit/secrets.toml); credentials
are read from that file and never printed. Output stays local in data/tbo/ (gitignored).

Usage (from the project root, with the data-fetcher venv):
  <data-fetcher>/.venv/Scripts/python.exe python/aireon_trial.py --probe
  <data-fetcher>/.venv/Scripts/python.exe python/aireon_trial.py --wef 2025-06-01 --til 2025-06-09
"""
import argparse
import tomllib
from pathlib import Path

import jaydebeapi
import pandas as pd

FETCHER = Path("C:/Users/rkoelle/dev/RProjects/data-fetcher")
JDBC_URL = ("jdbc:hive2://de-live-master0.az-live.x9er-zkvz.cloudera.site/;ssl=true;"
            "transportMode=http;httpPath=de-live/cdp-proxy-api/hive")
DRIVER_JAR = "C:/Users/rkoelle/hive-jdbc-uber-2.6.5.0-292.jar"
DRIVER_CLASS = "org.apache.hive.jdbc.HiveDriver"
TABLE = "gold_aireon.aireon_messages"
OUT = Path("data/tbo")


def connect():
    with open(FETCHER / ".streamlit" / "secrets.toml", "rb") as f:
        sec = tomllib.load(f)["hive"]
    return jaydebeapi.connect(DRIVER_CLASS, JDBC_URL, [sec["username"], sec["password"]], DRIVER_JAR)


def query(conn, sql):
    cur = conn.cursor()
    cur.execute(sql)
    cols = [d[0].split(".")[-1] for d in cur.description]
    df = pd.DataFrame(cur.fetchall(), columns=cols)
    cur.close()
    return df


def probe(conn):
    print(query(conn, f"DESCRIBE {TABLE}").to_string())
    print(query(conn, f"SELECT * FROM {TABLE} WHERE creation_time >= '2025-06-01' "
                      f"AND creation_time < '2025-06-01 00:05:00' LIMIT 5").T.to_string())


def extract(conn, wef, til):
    flights = pd.read_parquet(OUT / "aireon-trial-flights.parquet")
    icao = sorted({x.upper() for x in flights["ICAO24"].dropna()})
    parts = []
    for i in range(0, len(icao), 200):
        ids = ",".join(f"'{x}'" for x in icao[i:i + 200])
        sql = (f"SELECT * FROM {TABLE} WHERE creation_time >= '{wef}' AND creation_time < '{til}' "
               f"AND upper(a_080_aircraftaddress) IN ({ids})")
        df = query(conn, sql)
        print(f"chunk {i // 200 + 1}: {len(df)} rows")
        parts.append(df)
    data = pd.concat(parts, ignore_index=True)
    data.to_parquet(OUT / f"aireon-trial-{wef}-{til}.parquet")
    print(f"total {len(data)} rows, {data['a_080_aircraftaddress'].nunique()} aircraft")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--probe", action="store_true")
    ap.add_argument("--wef", default="2025-06-01")
    ap.add_argument("--til", default="2025-06-09")
    a = ap.parse_args()
    conn = connect()
    try:
        probe(conn) if a.probe else extract(conn, a.wef, a.til)
    finally:
        conn.close()
