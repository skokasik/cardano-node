#!/usr/bin/env nix-shell
#!nix-shell -p python3Packages.requests -i python3

import requests
import os
import time
import json

HYDRA_BASE_URL = "https://hydra.iohk.io"

BUILDKITE_BRANCH = os.getenv("BUILDKITE_BRANCH", None)
BUILDKITE_COMMIT = os.getenv("BUILDKITE_COMMIT", None)
BUILDKITE_PR = os.getenv("BUILDKITE_PULL_REQUEST", None)
BUILDKITE_REPO = os.getenv("BUILDKITE_PIPELINE_NAME", None)


if BUILDKITE_BRANCH == "bors/staging":
    hydra_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-bors-staging"
elif BUILDKITE_BRANCH == "bors/trying":
    hydra_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-bors-trying"
elif BUILDKITE_BRANCH == "master":
    hydra_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-master"
elif BUILDKITE_BRANCH == "develop":
    hydra_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-develop"
elif BUILDKITE_PR == "false":
    print("Please open a PR for hydra to evaluate")
    exit(1)
else:
    hydra_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-pr-{BUILDKITE_PR}"
    print(f"PR: {BUILDKITE_PR}")

hydra_eval = requests.get(hydra_url, headers={"Content-Type": "application/json"})

retry_count = 0
jobset_found = False
while not jobset_found:
    hydra_jobset = requests.get(hydra_url, headers={"Content-Type": "application/json"})
    if hydra_jobset.status_code == 200:
        jobset_found = True
        print("Jobset found - checking for evals")
    else:
        print("Hydra PR not created yet - sleeping 1 minute")
        retry_count = retry_count + 1
        time.sleep(60)
        if retry_count > 60:
            print("Retried 1 hour - exiting")
            exit(1)
        elif jobset_found:
            print("Jobset found - checking for evals")

eval_found = False
retry_count = 0
while not eval_found:
    hydra_evals = requests.get(f"{hydra_url}/evals", headers={"Content-Type": "application/json"})
    hydra_evals_data = json.loads(hydra_evals.text)
    for hydra_eval in hydra_evals_data["evals"]:
        if hydra_eval["jobsetevalinputs"][BUILDKITE_REPO]["revision"] == BUILDKITE_COMMIT:
            eval_found = True
    if not eval_found:
        print(f"Hydra eval not created yet for {BUILDKITE_COMMIT} - sleeping 1 minute")
        retry_count = retry_count + 1
        time.sleep(60)
        if retry_count > 60:
            print("Retried 1 hour - exiting")
            exit(1)

hydra_jobset_data = json.loads(hydra_jobset.text)
errormsg = hydra_jobset_data["errormsg"]

if errormsg != "":
    print(f"An error occurred in evaluation:\n{errormsg}")
    exit(1)

print("Evaluation completed without any errors")
exit(0)
