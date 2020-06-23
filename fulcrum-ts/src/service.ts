import { left, Either, right } from "fp-ts/lib/Either";
import { omit } from "ramda";

const apiUrl = "/apps/sweetspot/api";
const testMapEndpoint = "/bucket";
const eventEndpoint = "/event";
const logEndpoint = "/log";

type JsonEncodable = any;

interface OnlyCampaignId {
  kind: "only-campaign-id";
  campaignId: string;
}

interface OnlyUserId {
  kind: "only-user-id";
  userId: string;
}

interface UserAndCampaignId {
  kind: "user-and-campaign-id";
  campaignId: string;
  userId: string;
}

type TestMapProvisions = OnlyCampaignId | OnlyUserId | UserAndCampaignId;

const getTestMapQueryString = (
  testMapProvisions: TestMapProvisions,
): URLSearchParams => {
  switch (testMapProvisions.kind) {
    case "only-user-id":
      return new URLSearchParams(omit(["kind"], testMapProvisions));
    case "only-campaign-id":
      return new URLSearchParams(omit(["kind"], testMapProvisions));
    case "user-and-campaign-id":
      return new URLSearchParams(omit(["kind"], testMapProvisions));
  }
};

export const getTestMaps = async (
  testMapProvisions: TestMapProvisions,
): Promise<Array<TestMap>> => {
  const queryString = getTestMapQueryString(testMapProvisions);
  const res = await fetch(`${apiUrl}${testMapEndpoint}${queryString}`);
  return res.json();
};

const postJson = (url: string, body: JsonEncodable): Promise<Response> =>
  fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
  });

export const sendLog = (log: JsonEncodable): Promise<Either<Error, void>> =>
  postJson(`${apiUrl}${logEndpoint}`, log)
    .then(async response => {
      if (!response.ok) {
        const body = await response.json();
        return left(new Error(body.message));
      } else {
        return right(undefined);
      }
    })
    .catch(err => left(err));
