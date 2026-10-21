import os
import time
from time import gmtime, strftime
import pyam


def download_prisma(
    variables,
    scenarios,
    regions=None,
    prefix="download",
    outdir="data/direct",
    db="prisma-internal",
    meta=True,
    max_retries=4,
    retry_delay=30
):
    os.makedirs(outdir, exist_ok=True)

    print(f"Downloading: {prefix}")
    print(f"Variables: {variables}")

    kwargs = dict(
        name=db,
        scenario=scenarios,
        variable=variables,
        meta=meta
    )
    if regions is not None:
        kwargs["region"] = regions

    last_exc = None
    for attempt in range(1, max_retries + 1):
        try:
            df = pyam.read_iiasa(**kwargs)
            break
        except Exception as exc:
            last_exc = exc
            if attempt < max_retries:
                print(f"Attempt {attempt} failed ({type(exc).__name__}: {exc}). "
                      f"Retrying in {retry_delay}s …")
                time.sleep(retry_delay)
            else:
                print(f"All {max_retries} attempts failed for '{prefix}'.")
                raise last_exc

    timestr = strftime("%Y%m%d-%H%M%S", gmtime())
    filename = os.path.join(outdir, f"{prefix}_{timestr}.parquet")

    df.data.to_parquet(filename, index=False)

    print("done")
    print(f"Saved to: {filename}")
    print(f"Rows: {len(df.data):,}")

    return filename