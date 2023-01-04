
/* ------------------------------------------------------ */
/* meta                                                   */
/* ------------------------------------------------------ */

export type MetaType = 'string' | 'number' | 'factor' | 'date' | 'datetime' | 'href' | 'geo' | 'graph';
export type GraphDirection = 'none' | 'from' | 'to';
export type CurrencyCode = 'AED' | 'AFN' | 'ALL' | 'AMD' | 'ANG' | 'AOA' | 'ARS' | 'AUD' | 'AWG' | 'AZN' | 'BAM' | 'BBD' | 'BDT' | 'BGN' | 'BHD' | 'BIF' | 'BMD' | 'BND' | 'BOB' | 'BOV' | 'BRL' | 'BSD' | 'BTN' | 'BWP' | 'BYN' | 'BZD' | 'CAD' | 'CDF' | 'CHE' | 'CHF' | 'CHW' | 'CLF' | 'CLP' | 'CNY' | 'COP' | 'COU' | 'CRC' | 'CUC' | 'CUP' | 'CVE' | 'CZK' | 'DJF' | 'DKK' | 'DOP' | 'DZD' | 'EGP' | 'ERN' | 'ETB' | 'EUR' | 'FJD' | 'FKP' | 'GBP' | 'GEL' | 'GHS' | 'GIP' | 'GMD' | 'GNF' | 'GTQ' | 'GYD' | 'HKD' | 'HNL' | 'HRK' | 'HTG' | 'HUF' | 'IDR' | 'ILS' | 'INR' | 'IQD' | 'IRR' | 'ISK' | 'JMD' | 'JOD' | 'JPY' | 'KES' | 'KGS' | 'KHR' | 'KMF' | 'KPW' | 'KRW' | 'KWD' | 'KYD' | 'KZT' | 'LAK' | 'LBP' | 'LKR' | 'LRD' | 'LSL' | 'LYD' | 'MAD' | 'MDL' | 'MGA' | 'MKD' | 'MMK' | 'MNT' | 'MOP' | 'MRU' | 'MUR' | 'MVR' | 'MWK' | 'MXN' | 'MXV' | 'MYR' | 'MZN' | 'NAD' | 'NGN' | 'NIO' | 'NOK' | 'NPR' | 'NZD' | 'OMR' | 'PAB' | 'PEN' | 'PGK' | 'PHP' | 'PKR' | 'PLN' | 'PYG' | 'QAR' | 'RON' | 'RSD' | 'RUB' | 'RWF' | 'SAR' | 'SBD' | 'SCR' | 'SDG' | 'SEK' | 'SGD' | 'SHP' | 'SLE' | 'SLL' | 'SOS' | 'SRD' | 'SSP' | 'STN' | 'SVC' | 'SYP' | 'SZL' | 'THB' | 'TJS' | 'TMT' | 'TND' | 'TOP' | 'TRY' | 'TTD' | 'TWD' | 'TZS' | 'UAH' | 'UGX' | 'USD' | 'USN' | 'UYI' | 'UYU' | 'UYW' | 'UZS' | 'VED' | 'VES' | 'VND' | 'VUV' | 'WST' | 'XAF' | 'XAG' | 'XAU' | 'XBA' | 'XBB' | 'XBC' | 'XBD' | 'XCD' | 'XDR' | 'XOF' | 'XPD' | 'XPF' | 'XPT' | 'XSU' | 'XTS' | 'XUA' | 'XXX' | 'YER' | 'ZAR' | 'ZMW' | 'ZWL';

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

export interface ICurrencyMeta extends IMeta {
  code: CurrencyCode;
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

export type PanelFormat = 'apng' | 'avif' | 'gif' | 'jpg' | 'jpeg' | 'jfif' | 'pjpeg' | 'pjp' | 'png' | 'svg' | 'webp';

export type PanelType = 'img' | 'iframe' | 'REST';

export interface IDisplay {
  name: string;
  description: string;
  tags: string[];
  key_cols: string[];
  key_sig: string;
  metas: IMeta[];
  inputs: IInput[];
  state: IDisplayState;
  views: IView[];
  panel_type: PanelType;
  panel_format?: PanelFormat;
}

/* ------------------------------------------------------ */
/* display list                                           */
/* ------------------------------------------------------ */

export interface IDisplayListItem {
  name: string;
  description: string;
  tags: string[];
}

/* ------------------------------------------------------ */
/* config                                                 */
/* ------------------------------------------------------ */

export type AppDataType = 'jsonp' | 'json';

export interface IConfig {
  name: string;
  data_type: AppDataType;
  id: string;
}
