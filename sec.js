
// I AM DOING ARCHIVES

// phantomjs --web-security=false --ignore-ssl-errors=true --local-to-remote-url-access=true sec.js > sec.out.txt

// BUT MOST RECENT ...

// NOTE
// http://sec.gov/edgar/searchedgar/currentevents.htm
// Current Events: Most Recent: 10-Q Quarterly [SUBMIT]
// http://www.sec.gov/cgi-bin/current.pl?q1=0&q2=1&q3=
//
// ...The total number of matches for 2013-07-19 is 42
//
// ( TODAY: July 21, 2013 )
// Date Filed   Form           CIK Code     Company Name
// 07-19-2013  _10-Q_         _915913_      ALBEMARLE CORP

// _10-Q_: takes me to a regular 10-Q detail page
// e.g: http://www.sec.gov/Archives/edgar/data/915913/0001193125-13-295546-index.html

//
// From the result of the query: 'http://www.sec.gov/cgi-bin/browse-edgar?CIK=MSFT&Find=Search&owner=exclude&action=getcompany&count=100&type=10-Q';
var links     = [];
var linksOfInterest = [];

// From any particular 10-Q page with many types of files 
var importantlinks = [];
var importantLinksOfInterest = [];

var mybody    = [];

var base = 'http://www.sec.gov';
var subdirquerystringPRE_CIKticker = '/cgi-bin/browse-edgar?CIK=';
var ticker = 'MSFT';
var subdirquerystringPOST_CIKticker = '&Find=Search&owner=exclude&action=getcompany&count=100&type=10-Q';

// var subdirquerystring = '/cgi-bin/browse-edgar?CIK=MSFT&Find=Search&owner=exclude&action=getcompany&count=100&type=10-Q'
var subdirquerystring = subdirquerystringPRE_CIKticker + ticker + subdirquerystringPOST_CIKticker;

// var mainsite = 'http://www.sec.gov/cgi-bin/browse-edgar?CIK=MSFT&Find=Search&owner=exclude&action=getcompany&count=100&type=10-Q';
mainsite = base + '/' + subdirquerystring

// rem: closures do take global DEFAULTS
// rem: global needed to 'pass info from closure_local to closure_local'
// rem: global can not SEE closure_local

var is10Q = false;
var is10QA = false;

var inThousands = false;
var inMillions = false;

var thePeriodEnded;  // as text

var theRevenueRaw = 0.0;
var theNetIncomeRaw = 0.0;
var theEarningsPerShareBasicRaw = 0.0;
var theWeightedAveSharesBasicRaw = 0.0;
var theCashDivDeclPerCommonShareRaw = 0.0;

// NOTE: CASH_DIV_PER_SHARE * SHARE_COUNT = TOT_DIV_PAID
var theTotalDividendsPaidSELFCALCRaw = 0.0;

var theNetCashFromOperationsRaw = 0.0;

function getLinks() {
    var links = document.querySelectorAll('table.tableFile2 tbody tr td a');
    return Array.prototype.map.call(links, function(e) {
        return e.getAttribute('href')
    });
}

function getimportantLinks() {
    var importantlinks = document.querySelectorAll('table.tableFile tbody tr td a');
    return Array.prototype.map.call(importantlinks, function(e) {
        return e.getAttribute('href')
    });
}


phantom.casperPath = 'M:\\YDrive\\All_Economics\\eclipse_workspace\\headlessWebkit\\n1k0-casperjs-1.0.2-0-gbc0da16';
phantom.injectJs(phantom.casperPath + '\\bin\\bootstrap.js');

system = require('system');
utilities = require('utils');

var casper = require('casper').create( {
                                          // verbose: true,     
                                          // logLevel: 'debug',
                                          pageSettings: {
                                                  loadImages:  false,
                                                  loadPlugins: false
                                              }
                                         
                                       } );

casper.userAgent('Lynx/2.8.8dev.3 libwww-FM/2.14 SSL-MM/1.4.1');                                  
casper.start(); 

// used to verify that I am using UTF-8 
// console.log("Using default encoding..." + phantom.outputEncoding);


casper.then(function f_casper_open(){
	this.open(mainsite, function(response) {
    this.echo(response.data);
  });
});

casper.then(function f_concatechodumpLinks() {

     var li;

     links = links.concat(this.evaluate(getLinks));
              
     // this.echo(links.length + ' links found:'); 

     li = 0;
     for (var i in links) { 
       if(/Archives/.test(links[i])) { 
         linksOfInterest[li] = links[i]; 
         li = li + 1;
       }
     }
     // so resusable
     links = [];
     
     // utilities.dump(linksOfInterest); // O.K.
});

casper.then(function f_gotoLinks() {

     for (var i = 0; i < 1; i++) {
     // for (var i in linksOfInterest) { 

       this.thenOpen(base + linksOfInterest[i], function f__gotoCustomLink() {
            // this.echo(this.getTitle())
            
           
            this.then(function f___concatechodumpimportantLinks() {
            
                 var li;
                 var page_text;
                 

                 var match_result = [];
                 var match_result_item = [];
            
                 importantlinks = importantlinks.concat(this.evaluate(getimportantLinks));
                          
                 // this.echo(importantlinks.length + ' importantlinks found:'); 

                 li = 0;
                 for (var i in importantlinks) { 
                   if(/Archives/.test(importantlinks[i])) { 
                     importantLinksOfInterest[li] = importantlinks[i]; 
                     li = li + 1;
                   }
                 }
                 // so reusable
                 importantlinks = [];
                 
                 // utilities.dump(importantLinksOfInterest); // O.K.
                 
                 // Top link found is the most important - browse to it to read the 10-Q
                 this.thenOpen(base + importantLinksOfInterest[0], function f___gotoCustomLink() {

                      var thePeriodEnded_date = Date();
                 
                      // this.echo(this.getTitle());  // O.K.
                      // this.echo(this.getHTML());   // O.K.


                      // want to scrape a 10-Q and 'not a 10-Q/A'
                      if         ( /10\-Q\/A/gmi.test(page_text) ){
                           is10QA = true;
                      } else if  ( /10\-Q/gmi.test(page_text) ){
                           is10Q = true;
                      }

                      if ( !is10QA ) { 
                      
                          // HUMANLY READABLE when do " >> file.out.txt"            

                          page_text = this.fetchText('body');

                          // replace all WORTHLESS commas with nothing
                          page_text = page_text.replace(/,/gm,'');
                          
                          // replace all WORTHLESS dollar signs with a space
                          page_text = page_text.replace(/\$/gm,' ');

                          // replace all WORTHLESS carriage returns with a space
                          page_text = page_text.replace(/\n/gm,' ');
                          
                          // replace all 2+ CHAR WHITESPACE with a space
                          page_text = page_text.replace(/\s{2,}/gm,' ');
                          
                          // NOTE 1: ON WINDOWS ... MAKE SURE 'EOL' IS WINDOWS
                          // IF NOT, CAN WRECK REGULAR EXPRESSIONS
                          
                          // NOTE 2: Notepad++ and Notepad MAKE SURE SAVE IS UTF-8
                          
                          // NOTE 3:
                          // replace all 1 CHAR WHITESPACE with a single WHITESPACE
                          // A Regex for no-break space Unicode Entities
                          // http://www.perlmonks.org/?node_id=572690
                          // FIXES &nbsp; PROBLEM ... 
                          // BUT NOTE: &#150; ... ( hyphen? )   UNRESOLVED
                          // http://www.tedmontgomery.com/tutorial/htmlchrc.html
                          // NOT THE SAME AS
                          // http://www.asciitable.com/
                          page_text = page_text.replace(/\s/gm,' ');
                          
                          // this.echo(page_text);

                          if         ( /Millions/gmi.test(page_text) ){
                               inMillions = true;
                          } else if  ( /Thousands/gmi.test(page_text) ){
                               inThousands = true;
                          }

                          
                          // the phrase    Period Ended March 31 2013 - all Ele_1
                          match_result = page_text.match(/period\send[a-z]*\s[a-z]+\s\d{1,2}\s\d{4,4}/gmi)
                          // if ( !!match_result ) { 
                                // for ( var i in match_result ) {
                              // }
                          // }
                          
                          // word
                          match_result_item = match_result[0].split(" ");                          
                          // if ( !!match_result_item ) { 
                                // for ( var i in match_result_item ) {
                              // }
                          // }

                          // ticker in the output
                          this.echo(ticker);
                          
                          // Period Ended March 31 2013    
                          thePeriodEnded_date = new Date(Date.parse(match_result_item[2] + ' ' + match_result_item[3] + ' ' + match_result_item[4]));
                          // properly format a UTF date
                          thePeriodEnded = ( (thePeriodEnded_date.getDate() +  0) < 10 ? '0' + (thePeriodEnded_date.getDate() +  0)  : '' + (thePeriodEnded_date.getDate() +  0) ) + '-' + 
                                           ( (thePeriodEnded_date.getMonth() + 1) < 10 ? '0' + (thePeriodEnded_date.getMonth() + 1)  : '' + (thePeriodEnded_date.getMonth() + 1) ) + '-' + 
                                           thePeriodEnded_date.getFullYear();
                          this.echo(thePeriodEnded);
                          
                          
                          // Revenue
                          match_result = page_text.match(/Revenue\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");

                          
                          theRevenueRaw = match_result_item[1];
                          this.echo(theRevenueRaw);

                          // Net income
                          match_result = page_text.match(/Net\sincome\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");


                          theNetIncomeRaw = match_result_item[2];
                          this.echo(theNetIncomeRaw);

                          // Earnings per share: Basic
                          match_result = page_text.match(/Earnings\sper\sshare:?\sBasic\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");


                          theEarningsPerShareBasicRaw = match_result_item[4];
                          this.echo(theEarningsPerShareBasicRaw);

                          // Weighted average shares outstanding: Basic
                          match_result = page_text.match(/Weighted\saverage\sshares\soutstanding:?\sBasic\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");

                          theWeightedAveSharesBasicRaw = match_result_item[5];
                          this.echo(theWeightedAveSharesBasicRaw);

                          // Cash dividends declared per common share
                          match_result = page_text.match(/Cash\sdividends\sdeclared\sper\scommon\sshare\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");

                          theCashDivDeclPerCommonShareRaw = match_result_item[6];
                          this.echo(theCashDivDeclPerCommonShareRaw);

                          // Net cash from operations
                          // NOTE FOUND FIRST AND IGNORED ( NOT MATCHED ): net cash from operations: Depreciation:
                          match_result = page_text.match(/Net\scash\sfrom\soperations\s\d+.?\d+/gmi);
                          match_result_item = match_result[0].split(" ");

                          theNetCashFromOperationsRaw = match_result_item[4];
                          this.echo(theNetCashFromOperationsRaw);

                          // 31-3-2013
                          // 20489
                          // 6055
                          // 0.72
                          // 8364
                          // 0.23
                          // 9666
                          
                          
                          
                      } ;
                      
                 });
                 
            });
            
       });

     }

})




casper.run(function f_steps_map() {
  // this.echo('RUNNING');
  // Display function calls
  // utilities.dump(casper.steps.map(function(step) { return step.toString(); }));
  // Display function calls better
  // utilities.dump(casper.steps.map(function(step) { return step.toString().substring(0,step.toString().search(/\(/)); }));
  // this.echo('RUNNING_DONE');
	this.exit();
});


