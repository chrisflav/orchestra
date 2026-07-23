import { createContext, useContext } from "react";
import type { ReactNode } from "react";
import type { Overview } from "./api";
import type { LiveData } from "./useLiveData";
import { useLiveData } from "./useLiveData";

/**
 * One subscription to `overview`, shared by the rail and the Overview page.
 *
 * The rail shows live counts on every page, so it needs this data everywhere. Letting the
 * Overview page open its own stream would mean two connections to the same endpoint whenever
 * that page is open, and two copies of the payload arriving two seconds apart.
 */
const OverviewContext = createContext<LiveData<Overview> | null>(null);

export function OverviewProvider({ children }: { children: ReactNode }) {
  const live = useLiveData("overview");
  return <OverviewContext.Provider value={live}>{children}</OverviewContext.Provider>;
}

export function useOverview(): LiveData<Overview> {
  const ctx = useContext(OverviewContext);
  if (ctx === null) throw new Error("useOverview must be used within an OverviewProvider");
  return ctx;
}
