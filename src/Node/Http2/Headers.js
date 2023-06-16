import { sensitiveHeaders } from "node:http2";

export const mkHeadersImpl = (insensitive, sensitive, sensitiveKeys) => 
  ({
    ...insensitive,
    ...sensitive,
    [sensitiveHeaders]: sensitiveKeys,
  });
