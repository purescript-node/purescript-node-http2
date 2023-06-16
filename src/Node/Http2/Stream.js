export const bufferSizeImpl = (stream) => stream.bufferSize;
export const closeImpl = (stream, code) => stream.close(code);
export const closedImpl = (stream) => stream.closed;
export const destroyedImpl = (stream) => stream.destroyed;
export const endAfterHeadersImpl = (s) => s.endAfterHeaders;
export const idImpl = (s) => s.id;
export const pendingImpl = (s) => s.pending;
export const priorityImpl = (s, p) => s.priority(p);
export const rstCodeImpl = (s) => s.rstCode;
export const sentHeadersImpl = (s) => s.sentHeaders;
export const sentInfoHeadersImpl = (s) => s.sentInfoHeaders;
export const sentTrailersImpl = (s) => s.sentTrailers;
export const sessionImpl = (s) => s.session;
export const setTimeoutImpl = (s, msecs, cb) => s.setTimeout(msecs, cb);
export const stateImpl = (s) => s.state;
export const sendTrailersImpl = (s, t) => s.sendTrailers(t);

// server

export const additionalHeadersImpl = (s, h) => s.additionalHeaders(h);
export const headersSentImpl = (s) => s.headersSent;
export const pushAllowedImpl = (s) => s.pushAllowed;
export const pushStreamNoOptionsImpl = (s, headers, cb) => s.pushStream(headers, cb);
export const pushStreamOptionsImpl = (s, headers, options, cb) => s.pushStream(headers, options, cb);
export const respondImpl = (s, headers, options) => s.respond(headers, options);
export const respondWithFdImpl = (s, fd, h, o) => s.respondWithFd(fd, h, o);
export const respondWithFileImpl = (s, file, headers, o) => s.respondWithFile(file, headers, o);

// client

