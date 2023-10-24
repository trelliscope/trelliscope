import {
  // meta
  IMeta,
  IPanelMeta,
  INumberMeta,
  ICurrencyMeta,
  IStringMeta,
  IFactorMeta,
  IDateMeta,
  IDatetimeMeta,
  IHrefMeta,
  IGeoMeta,
  IGraphMeta,
  MetaType,
  GraphDirection,
  CurrencyCode,
  // inputs
  IInputClientSideStorage,
  IInputEmailFeedback,
  IInputs,
  IInput,
  IRadioInput,
  ICheckboxInput,
  ISelectInput,
  IMultiselectInput,
  ITextInput,
  INumberInput,
  InputType,
  // state
  IDisplayState,
  IState,
  ViewType,
  ILayoutState,
  ILabelState,
  ISortState,
  IFilterState,
  ICategoryFilterState,
  INumberRangeFilterState,
  IDateRangeFilterState,
  IDatetimeRangeFilterState,
  StateType,
  LayoutArrangeType,
  SortDirType,
  FilterType,
  // view
  IView,
  // display
  IDisplay,
  PanelFormat,
  PanelType,
  PanelSourceType,
  IPanelSource,
  IFilePanelSource,
  IRESTPanelSource,
  ILocalWebSocketPanelSource,
  // display list
  IDisplayListItem,
  // config
  AppDataType,
  IConfig,
} from "./types";

/* ------------------------------------------------------ */
/* panel                                                  */
/* ------------------------------------------------------ */

export class PanelSource implements IPanelSource {
  type: PanelSourceType;
  constructor(
    type: PanelSourceType,
  ) {
    this.type = type;
  }
}

export class FilePanelSource extends PanelSource implements IFilePanelSource {
  constructor() { super('file') };
}

export class RESTPanelSource extends PanelSource implements IRESTPanelSource {
  url: string;
  apiKey: string | undefined;
  headers: string | undefined;
  constructor(
    {
      url,
      apiKey,
      headers,
    } : {
      url: string;
      apiKey: string | undefined;
      headers: string | undefined;    
    }
  ) {
    super('REST');
    this.url = url;
    this.apiKey = apiKey;
    this.headers = headers;
  }
}

export class LocalWebSocketPanelSource extends PanelSource implements ILocalWebSocketPanelSource {
  port: number;
  constructor(
    {
      port
    } : {
      port: number;
    }
  ) {
    super('REST');
    this.port = port;
  }
}

/* ------------------------------------------------------ */
/* meta                                                   */
/* ------------------------------------------------------ */

export class Meta implements IMeta {
  type: MetaType;
  varname: string;
  label: string;
  tags: string[];
  filterable: boolean;
  sortable: boolean;
  maxnchar: number;
  constructor(
    type: MetaType,
    varname: string,
    tags: string[] = [],
    label: string | undefined = undefined,
    filterable: boolean = true,
    sortable: boolean = true,
    maxnchar: number
  ) {
    this.type = type;
    this.varname = varname;
    this.tags = tags;
    this.filterable = filterable;
    this.sortable = sortable;
    this.label = label === undefined ? varname : label;
    this.maxnchar = maxnchar;
  }
}

export class PanelMeta extends Meta implements IPanelMeta {
  paneltype: PanelType;
  format?: PanelFormat;
  aspect: number;
  source: PanelSource;
  constructor(
    {
      varname,
      label,
      tags,
      paneltype,
      format,
      aspect,
      source,
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[],
      paneltype: PanelType,
      format: PanelFormat | undefined,
      aspect: number,
      source: PanelSource,
    }
  ) {
    super('panel', varname, tags, label, true, true, 0);
    this.paneltype = paneltype;
    if (format !== undefined) {
      this.format = format;
    }
    this.aspect = aspect;
    this.source = source;
  }
}

export class NumberMeta extends Meta implements INumberMeta {
  digits: number;
  locale: boolean;
  log: boolean;
  constructor(
    {
      varname,
      label,
      tags,
      digits,
      locale,
      log
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[],
      digits?: number,
      locale?: boolean,
      log?: boolean,
    }
  ) {
    super('number', varname, tags, label, true, true, 0);
    this.digits = digits === undefined ? 2 : digits;
    this.locale = locale === undefined ? true : locale;
    this.log = log === undefined ? false : log;
  };
}

export class CurrencyMeta extends Meta implements ICurrencyMeta {
  code: CurrencyCode;
  digits: number;
  log: boolean;
  constructor(
    {
      varname,
      label,
      tags,
      code,
      digits,
      log,
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[],
      code?: CurrencyCode,
      digits?: number,
      log?: boolean,
    }
  ) {
    super('number', varname, tags, label, true, true, 0);
    this.code = code === undefined ? 'USD' : code;
    this.digits = digits === undefined ? 2 : digits;
    this.log = log === undefined ? false : log;
  };
}

export class StringMeta extends Meta implements IStringMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'string',
      varname,
      tags,
      label,
      true,
      true,
      0
    );
  };
}

export class FactorMeta extends Meta implements IFactorMeta {
  levels: string[];
  constructor(
    { 
      varname,
      levels,
      label,
      tags
    }: {
      varname: string,
      levels: string[],
      label?: string | undefined,
      tags?: string[],
    }
  ) {
    super(
      'factor',
      varname,
      tags,
      label,
      true,
      true,
      0
    );
    this.levels = levels;
  };
}

export class DateMeta extends Meta implements IDateMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'date',
      varname,
      tags,
      label,
      true,
      true,
      0
    );
  };
}

export class DatetimeMeta extends Meta implements IDatetimeMeta {
  timezone: string;
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'datetime',
      varname,
      tags,
      label,
      true,
      true,
      0
    );
    this.timezone = 'UTC';
  };
}

export class GeoMeta extends Meta implements IGeoMeta {
  latvar: string;
  longvar: string;
  constructor(
    {
      varname,
      latvar,
      longvar,
      label,
      tags
    } : {
      varname: string,
      latvar: string,
      longvar: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'geo',
      varname,
      tags,
      label,
      false, // TODO: change to TRUE when implemented in app
      false,
      0
    );
    this.latvar = latvar;
    this.longvar = longvar;
  };
}

export class HrefMeta extends Meta implements IHrefMeta {
  constructor(
    {
      varname,
      label,
      tags
    } : {
      varname: string,
      label?: string | undefined,
      tags?: string[]
    }
  ) {
    super(
      'href',
      varname,
      tags,
      label,
      false,
      false,
      0
    );
  };
}

export class GraphMeta extends Meta implements IGraphMeta {
  idvarname: string;
  linkidvarname: string;
  labelvarname: string;
  params: object;
  direction: GraphDirection;
  constructor(
    {
      varname,
      label,
      tags,
      idvarname,
      linkidvarname,
      labelvarname,
      // params,
      direction
    } : {
      varname: string,
      linkidvarname: string,
      labelvarname?: string,
      params?: object,
      label?: string | undefined,
      tags?: string[],
      idvarname: string,
      direction?: GraphDirection
    }
  ) {
    super(
      'graph',
      varname,
      tags,
      label,
      true,
      false,
      0
    );
    this.idvarname = idvarname;
    this.linkidvarname = linkidvarname;
    this.labelvarname = labelvarname === undefined ? idvarname : labelvarname;
    this.params = {};
    this.direction = direction === undefined ? 'none' : direction;
  };
}

/* ------------------------------------------------------ */
/* inputs                                                 */
/* ------------------------------------------------------ */


export class InputClientSideStorage implements IInputClientSideStorage {
  type: "localStorage";
  constructor() {
    this.type = "localStorage";
  }
}

export class InputEmailFeedback implements IInputEmailFeedback {
  emailAddress: string;
  includeMetaVars: string[];
  constructor(
    {
      emailAddress,
      includeMetaVars,
    } : {
      emailAddress: string,
      includeMetaVars: string[],
    }
  ) {
    this.emailAddress = emailAddress;
    this.includeMetaVars = includeMetaVars;
  }
}

export class Inputs implements IInputs {
  inputs: IInput[];
  storageInterface: IInputClientSideStorage;
  feedbackInterface: IInputEmailFeedback;
  constructor(
    {
      inputs,
      storageInterface,
      feedbackInterface,
    } : {
      inputs: IInput[],
      storageInterface: IInputClientSideStorage,
      feedbackInterface: IInputEmailFeedback,
    }
  ) {
    this.inputs = inputs;
    this.storageInterface = storageInterface;
    this.feedbackInterface = feedbackInterface;
  }
}

export class Input implements IInput {
  type: InputType;
  name: string;
  label: string;
  active: boolean;
  constructor(
    type: InputType,
    name: string,
    label: string | undefined = undefined,
    active: boolean | undefined = true,
  ) {
    this.type = type;
    this.name = name;
    this.active = active === undefined ? true: active;
    this.label = label === undefined ? name: label;
  }
}

export class RadioInput extends Input implements IRadioInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('radio', name, label, active);
    this.options = options;
  };
}

export class CheckboxInput extends Input implements ICheckboxInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('checkbox', name, label, active);
    this.options = options;
  };
}

export class SelectInput extends Input implements ISelectInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('select', name, label, active);
    this.options = options;
  };
}

export class MultiselectInput extends Input implements IMultiselectInput {
  options: string[];
  constructor(
    {
      name,
      label,
      active,
      options
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      options: string[]
    }
  ) {
    super('multiselect', name, label, active);
    this.options = options;
  };
}

export class TextInput extends Input implements ITextInput {
  height: number
  constructor(
    {
      name,
      label,
      active,
      height,
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      height?: number | undefined,
    }
  ) {
    super('text', name, label, active);
    this.height = height === undefined ? 3 : height;
  };
}

export class NumberInput extends Input implements INumberInput {
  min: number | null;
  max: number | null;
  constructor(
    {
      name,
      label,
      active,
      min,
      max,
    } : {
      name: string,
      label?: string | undefined,
      active?: boolean | undefined,
      min?: number | undefined,
      max?: number | undefined,
    }
  ) {
    super('number', name, label, active);
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

/* ------------------------------------------------------ */
/* states                                                 */
/* ------------------------------------------------------ */

export class DisplayState implements IDisplayState {
  layout: ILayoutState;
  labels: ILabelState;
  sort: ISortState[];
  filter: IFilterState[];
  filterView: string[];
  constructor(
    {
      layout,
      labels,
      sort,
      filter,
      filterView
    } : {
      layout?: ILayoutState | undefined,
      labels: ILabelState,
      sort?: ISortState[] | undefined,
      filter?: IFilterState[] | undefined,
      filterView?: string[] | undefined,
    }
  ) {
    this.layout = layout === undefined ? new LayoutState({}) : layout;
    this.labels = labels;
    this.sort = sort === undefined ? [] : sort;
    this.filter = filter === undefined ? [] : filter;
    this.filterView = filterView === undefined ? [] : filterView;
  }
}

export class State implements IState {
  type: StateType;
  constructor(
    type: StateType,
  ) {
    this.type = type;
  }
}

export class LayoutState extends State implements ILayoutState {
  ncol: number;
  page: number;
  viewtype: ViewType;
  sidebarActive: boolean;
  constructor(
    {
      ncol,
      page,
      viewtype,
      sidebarActive,
    } : {
      ncol?: number | undefined,
      page?: number | undefined,
      viewtype?: ViewType | undefined,
      sidebarActive?: boolean | undefined,
    }
  ) {
    super('layout');
    this.ncol = ncol === undefined ? 1 : ncol;
    this.page = page === undefined ? 1 : page;
    this.sidebarActive = sidebarActive === undefined ? false : sidebarActive;
    this.viewtype = viewtype === undefined ? 'grid' : viewtype;
  };
}

export class LabelState extends State implements ILabelState {
  varnames: string[];
  constructor(
    {
      varnames,
    } : {
      varnames: string[],
    }
  ) {
    super('labels');
    this.varnames = varnames;
  };
}

export class SortState extends State implements ISortState {
  varname: string;
  dir: SortDirType;
  metatype: MetaType;
  constructor(
    {
      varname,
      dir,
      metatype,
    } : {
      varname: string,
      dir?: SortDirType | undefined,
      metatype: MetaType,
    }
  ) {
    super('sort');
    this.varname = varname;
    this.dir = dir === undefined ? 'asc' : dir;
    this.metatype = metatype;
  };
}

export class FilterState extends State implements IFilterState {
  varname: string;
  filtertype: FilterType;
  metatype: MetaType;
  constructor(
    {
      varname,
      filtertype,
      metatype,
    } : {
      varname: string,
      filtertype: FilterType,
      metatype: MetaType,
    }
  ) {
    super('filter');
    this.varname = varname;
    this.filtertype = filtertype;
    this.metatype = metatype;
  };
}

export class CategoryFilterState extends FilterState implements ICategoryFilterState {
  regexp: string | null;
  values: string[];
  constructor(
    {
      varname,
      regexp,
      values,
      metatype,
    } : {
      varname: string,
      regexp?: string | undefined,
      values?: string[] | undefined,
      metatype: MetaType,
    }
  ) {
    super({ varname, filtertype: 'category', metatype });
    this.regexp = regexp === undefined ? null : regexp;
    this.values = values === undefined ? [] : values;
  };
}

export class NumberRangeFilterState extends FilterState implements INumberRangeFilterState {
  min: number | null;
  max: number | null;
  constructor(
    {
      varname,
      min,
      max,
      metatype,
    } : {
      varname: string,
      min?: number | undefined,
      max?: number | undefined,
      metatype?: MetaType,
    }
  ) {
    super({
      varname,
      filtertype: 'numberrange',
      metatype: metatype === undefined ? 'number' : metatype,
    });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

export class DateRangeFilterState extends FilterState implements IDateRangeFilterState {
  min: Date | null;
  max: Date | null;
  constructor(
    {
      varname,
      min,
      max,
      metatype,
    } : {
      varname: string,
      min?: Date | undefined,
      max?: Date | undefined,
      metatype?: MetaType,
    }
  ) {
    super({
      varname,
      filtertype: 'daterange',
      metatype: metatype === undefined ? 'date' : metatype,
    });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

export class DatetimeRangeFilterState extends FilterState implements IDatetimeRangeFilterState {
  min: Date | null;
  max: Date | null;
  constructor(
    {
      varname,
      min,
      max,
      metatype,
    } : {
      varname: string,
      min?: Date | undefined,
      max?: Date | undefined,
      metatype?: MetaType,
    }
  ) {
    super({
      varname,
      filtertype: 'datetimerange',
      metatype: metatype === undefined ? 'datetime' : metatype,
    });
    this.min = min === undefined ? null : min;
    this.max = max === undefined ? null : max;
  };
}

/* ------------------------------------------------------ */
/* view                                                   */
/* ------------------------------------------------------ */

export class View implements IView {
  name: string;
  state: IDisplayState;
  constructor(
    {
      name,
      state,
    } : {
      name: string,
      state: IDisplayState,
    }
  ) {
    this.name = name;
    this.state = state;
  }
}

/* ------------------------------------------------------ */
/* display                                                */
/* ------------------------------------------------------ */

export class Display implements IDisplay {
  name: string;
  description: string;
  tags: string[];
  keycols: string[];
  keysig: string;
  metas: IMeta[];
  inputs: IInputs | null;
  state: IDisplayState;
  views: IView[];
  primarypanel: string;
  thumbnailurl: string;
  order: number;
  constructor(
    {
      name,
      description,
      tags,
      keycols,
      keysig,
      metas,
      inputs,
      state,
      views,
      primarypanel,
      thumbnailurl,
      order,
    } : {
      name: string,
      description?: string,
      tags?: string[],
      keycols: string[],
      keysig: string,
      metas: IMeta[],
      inputs?: IInputs | null,
      state: IDisplayState,
      views?: IView[] | undefined,
      primarypanel: string,
      thumbnailurl: string,
      order: number | undefined,
    }
  ) {
    this.name = name;
    this.description = description === undefined ? name : description;
    this.tags = tags === undefined ? [] : tags;
    this.keycols = keycols;
    this.keysig = keysig;
    this.metas = metas;
    this.inputs = inputs === undefined ? null : inputs;
    this.state = state;
    this.views = views === undefined ? [] : views;
    this.primarypanel = primarypanel;
    this.thumbnailurl = thumbnailurl;
    this.order = order === undefined ? 0 : order;
  }
}

/* ------------------------------------------------------ */
/* display list                                           */
/* ------------------------------------------------------ */

export class DisplayListItem implements IDisplayListItem {
  name: string;
  description: string;
  tags: string[];
  thumbnailurl: string;
  constructor(
    {
      name,
      description,
      tags,
      thumbnailurl,
    } : {
      name: string,
      description?: string,
      tags?: string[],
      thumbnailurl: string,
    }
  ) {
    this.name = name;
    this.description = description === undefined ? name : description;
    this.tags = tags === undefined ? [] : tags;
    this.thumbnailurl = thumbnailurl;
  }
}

// var displayList: DisplayListItem[] = [...]

/* ------------------------------------------------------ */
/* config                                                 */
/* ------------------------------------------------------ */

export class Config implements IConfig {
  name: string;
  datatype: AppDataType;
  id: string;
  constructor(
    {
      name,
      datatype,
      id,
    } : {
      name: string;
      datatype: AppDataType;
      id: string;
    }
  ) {
    this.name = name;
    this.datatype = datatype;
    this.id = id;
  }
}
