
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
  digits: number; // should be integer (less than 0 means show all digits)
  log: boolean;
  locale: boolean;
}

export interface ICurrencyMeta extends IMeta {
  code: CurrencyCode;
  digits: number; // should be integer (less than 0 means show all digits)
  log: boolean;
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
  latvar: string;
  longvar: string;
}

export interface IGraphMeta extends IMeta {
  idvarname: string;
  linkidvarname: string;
  labelvarname: string;
  params: object;
  direction: GraphDirection;
}

/* ------------------------------------------------------ */
/* inputs                                                 */
/* ------------------------------------------------------ */

// how the inputs will be stored (client side doesn't have any properties)
export interface IInputClientSideStorage {
  type: "localStorage"
}

// how the inputs will be relayed back to the creator of the display
export interface IInputEmailFeedback {
  emailAddress: string;
  includeMetaVars: string[];
}

export interface IInputs {
  inputs: IInput[];
  storageInterface: IInputClientSideStorage;
  feedbackInterface: IInputEmailFeedback;
}

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
  ncol: number;
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

export type PanelType = 'img' | 'iframe';

export type PanelSourceType = 'file' | 'REST' | 'localWebSocket';

export interface IPanelSource {
  type: PanelSourceType;
}

export interface IFilePanelSource extends IPanelSource {
}

export interface IRESTPanelSource extends IPanelSource {
  url: string;
  apiKey: string | undefined;
  headers: string | undefined;
}

export interface ILocalWebSocketPanelSource extends IPanelSource {
  port: number;
}

export interface IDisplay {
  name: string;
  description: string;
  tags: string[];
  keycols: string[];
  keysig: string;
  metas: IMeta[];
  inputs: IInputs | null;
  state: IDisplayState;
  views: IView[];
  paneltype: PanelType;
  panelformat?: PanelFormat;
  panelaspect: number,
  panelsource: IPanelSource,
  thumbnailurl: string;
}

/* ------------------------------------------------------ */
/* display list                                           */
/* ------------------------------------------------------ */

export interface IDisplayListItem {
  name: string;
  description: string;
  tags: string[];
  keysig: string;
  thumbnailurl: string;
}

/* ------------------------------------------------------ */
/* config                                                 */
/* ------------------------------------------------------ */

export type AppDataType = 'jsonp' | 'json';

export interface IConfig {
  name: string;
  datatype: AppDataType;
  id: string;
}
