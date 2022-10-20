export type MetaType = 'string' | 'number' | 'factor' | 'date' | 'datetime' | 'href' | 'geo' | 'graph';
export type GraphDirection = 'none' | 'from' | 'to';

export interface IMeta {
  varname: string;
  type: MetaType;
  label: string;
  tags: string[];
  filterable: boolean;
  sortable: boolean;
}

export interface INumberMeta extends IMeta {
  digits: number | null; // should be integer
  locale: boolean;
}

export interface IStringMeta extends IMeta {
}

export interface IFactorMeta extends IMeta {
  levels: string[];
}

export interface IDateMeta extends IMeta {
}

export interface IDatetimeMeta extends IMeta {
  timezone: string;
}

export interface IHrefMeta extends IMeta {
}

export interface IGeoMeta extends IMeta {
}

export interface IGraphMeta extends IMeta {
  idvarname: string,
  direction: GraphDirection
}

