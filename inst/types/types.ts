
/* ------------------------------------------------------ */
/* meta                                                   */
/* ------------------------------------------------------ */

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

// do we want to support this?
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
export interface IDatetimeMeta extends IMeta {
  timezone: string;
}

export interface IHrefMeta extends IMeta {
}

export interface IGeoMeta extends IMeta {
}

export interface IGraphMeta extends IMeta {
  idvarname: string;
  direction: GraphDirection;
}

/* ------------------------------------------------------ */
/* inputs                                                 */
/* ------------------------------------------------------ */

export type InputType = 'radio' | 'checkbox' | 'select' | 'multiselect' | 'text' | 'number';

export interface IInput {
  name: string;
  label: string;
  active: boolean;
  type: InputType;
}

export interface IRadioInput extends IInput {
  options: string[];
}

export interface ICheckboxInput extends IInput {
  options: string[];
}

export interface ISelectInput extends IInput {
  options: string[];
}

export interface IMultiselectInput extends IInput {
  options: string[];
}

export interface ITextInput extends IInput {
  width: number;
  height: number;
}

export interface INumberInput extends IInput {
}

/* ------------------------------------------------------ */
/* states                                                 */
/* ------------------------------------------------------ */

export type StateType = 'layout' | 'labels' | 'sort' | 'filter';
export type LayoutArrangeType = 'rows' | 'cols';
export type SortDirType = 'asc' | 'desc';
export type FilterType = 'category' | 'numberrange' | 'daterange' | 'datetimerange';

export interface IDisplayState {
  layout: ILayoutState;
  labels: ILabelState;
  sort: ISortState[];
  filter: IFilterState[];
}

export interface IState {
  type: StateType;
}

export interface ILayoutState extends IState {
  nrow: number;
  ncol: number;
  arrange: LayoutArrangeType;
  page: number;
}

export interface ILabelState extends IState {
  varnames: string[];
}

export interface ISortState extends IState {
  varname: string;
  dir: SortDirType;
}

export interface IFilterState extends IState {
  varname: string;
  filtertype: FilterType;
}

export interface ICategoryFilterState extends IFilterState {
  regexp: string | null;
  values: string[];
}

export interface INumberRangeFilterState extends IFilterState {
  min: number | null;
  max: number | null;
}

export interface IDateRangeFilterState extends IFilterState {
  min: Date | null;
  max: Date | null;
}

export interface IDatetimeRangeFilterState extends IFilterState {
  min: Date | null;
  max: Date | null;
}

/* ------------------------------------------------------ */
/* view                                                   */
/* ------------------------------------------------------ */

export interface IView {
  name: string;
  state: IDisplayState;
}

/* ------------------------------------------------------ */
/* display                                                */
/* ------------------------------------------------------ */

export interface IDisplay {
  name: string;
  description: string;
  id_vars: string[];
  metas: IMeta[];
  inputs: IInput[];
  state: IDisplayState;
  views: IView[];
}
