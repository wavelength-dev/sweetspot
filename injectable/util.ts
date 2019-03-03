export const isEmpty = (array: Array<unknown>) =>
  Array.isArray(array) && array.length > 0

export const isElementsFound = (els: HTMLCollection) => els.length > 0
