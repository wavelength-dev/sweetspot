import whenDomReady from "when-dom-ready";
import pLimit from "p-limit";
const getIsRuntimeAdequate = () => {
  const isFetchRunnable =
    typeof window !== "undefined" && typeof window.fetch !== "undefined";
  const isPromiseRunnable =
    typeof window !== "undefined" && typeof window.Promise !== "undefined";
  return isFetchRunnable && isPromiseRunnable;
};

const fetchTests = () => fetch("http://localhost:8082/api/bucket");

const getTestMap = async () => {
  await whenDomReady();
  const isRuntimeAdequate = getIsRuntimeAdequate();
  if (!isRuntimeAdequate) {
    throw new Error("Inadequate runtime!");
  }

  const tests = await fetchTests();

  console.log("bucket", tests);

  return new Map();
};

const limit = pLimit(1);
const apply = () =>
  limit(async () => {
    const testMap = await getTestMap();
    console.log("Map size", testMap.size);
  });

// Globally exported functions
(window as any).SweetSpot = {
  apply,
};
