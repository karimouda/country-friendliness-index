/**
 * This file is used to fetch data from Google Maps Places API and output a CSV file.
 *
 *
 * @author Karim Ouda.
 * @since  05.11.2019
 */

// The api key to be used by google to track your apis call and charge you money
// You can get this key through this page: https://developers.google.com/places/web-service/get-api-key
const apiKey = 'ADD_YOUR_GOOGLE_CLOUD_API_KEY'

// imports
const urlrequest = require('request-promise-native');
const sleep = require('sleep');
const fs = require('fs');


/*
*  prepares CSV strings for output
*/
function clean_n_quote(str){
  return "\""+cleanReviewText(str)+"\"";
}

/*
*  Clean Review text
*/
function cleanReviewText(str){
  if (str==undefined){ return "" }
  else{
      return str.toString().replace(/"/g,"'").replace(/\n|\r/g, " ");
  }


}


/*
*  Writes CSV content to file
*/
function writeCSVtoFile(file_path,csvContent){


  fs.appendFile(file_path, csvContent,'utf8', function(err) {

      if(err) {
          return console.log(err);
      }

  });


}



/*
*  Get countries list from JSON file
*/
async function getCountriesList(file_path){


  //File from https://raw.githubusercontent.com/annexare/Countries/master/dist/countries.json
  let jsonArr = JSON.parse(fs.readFileSync(file_path, 'utf8'));

  console.log(jsonArr);

  return jsonArr;
}





/*
* Get reviews for a specific google maps place id
*/
async function getReviews(place_id){

    console.log("----- getReviews");

    // Generate and fill Place Details API url
    let gm_place_details_api = 'https://maps.googleapis.com/maps/api/place/details/json?fields=name,rating,reviews';

    gm_place_details_api  = gm_place_details_api + '&key='+apiKey+'&placeid='+place_id;

    //console.log("URL:"+gm_place_details_api);


    var call_options = {
        uri : gm_place_details_api,
        method : 'GET',
        headers :  {'content-type': 'application/json'}
    };

    // Call Places API
    let api_response =  await urlrequest(call_options);

    //console.log(api_response);

    let result_object = JSON.parse(api_response);

    //console.log("All Reviews:"+JSON.stringify(result_object.result.reviews));

    // Result is not an error
    if (result_object.result!=undefined &&
        result_object.result.reviews != undefined &&
        result_object.result.reviews.length >= 0 )
    {
      console.log("Result Size:"+result_object.result.reviews.length);
      return result_object.result.reviews;
    }else{
      console.log("No Reviews returned.");
      return false;
    }


}



/*
* Get google maps places for a specific location query
* maxResults can't be more than 60
* https://developers.google.com/places/web-service/search
*/
async function getPlaces(q,type){

  console.log("----- getPlaces");

  console.log("Query:"+q);
  console.log("Type:"+type);


  // Generate and fill Place text search API url
  let gm_place_api_url_base = 'https://maps.googleapis.com/maps/api/place/textsearch/json?';

  let gm_place_api_url  = gm_place_api_url_base + 'key='+apiKey+'&query='+q+'&inputtype=textquery&type='+type

  gm_place_api_url  = gm_place_api_url + '&fields=id,place_id,formatted_address,name,rating,geometry';

  let maxResults = 60;

  // Get only 3 pages (20 place each) for each location
  let rounds = Math.round(maxResults/20);

  console.log("Rounds:"+rounds);

  let result_obj = null;
  let api_response = null;
  let all_results = null;

  for(let i =0;i<rounds;i++){

    console.log("Calling URL:"+gm_place_api_url);


    var call_options = {
        uri : gm_place_api_url,
        method : 'GET',
        headers :  {'content-type': 'application/json'}
    };


    let api_response =  await urlrequest(call_options);


    let result_obj = JSON.parse(api_response);


    console.log("Size:"+result_obj.results.length);

    // First round, then add, else append results
    if (i==0){
      all_results = result_obj.results;
    }
    else{

      all_results = all_results.concat(result_obj.results);
    }

    // next page token exists
    if (result_obj.next_page_token!=undefined &&
        result_obj.next_page_token!=null ){

      // set the URL of the next page
      gm_place_api_url = gm_place_api_url_base+'pagetoken='+result_obj.next_page_token+'&key='+apiKey;

      // wait 2 seconds
      sleep.sleep(2);
    }else{
      // no page token ? then stop looping
      break;
    }




  }

  console.log("Aggregated Results Count:"+all_results.length+"\n");

  // Aggregated results object quality checks
  if (all_results.length !=undefined &&
      all_results.length > 0){
    return all_results;
  }else{
    return false;
  }

}




async function main(){

    const cities_file = "./countries.json";

    // Get list of countries as JSON object
    let countries_list  = await getCountriesList(cities_file);
    console.log(countries_list)

    // constants
    let outputCSV = "continent,country,capital,place_id,place_name,place_type,place_review_score,number_of_reviewers,text_of_five_reviews\n"
    let output_file_path = 'country-fi-reviews.csv';


    // write CSV file header (titles)
    writeCSVtoFile(output_file_path,outputCSV);


    // to count API calls for cost prediction reasons // next page calls in Place textsearch API is not included
    let api_counter = 0;


    // For each country
    for(country_id in countries_list){



      try{
            country = countries_list[country_id];



            let country_name = country.name;
            let capital_name =  country.capital;
            let continent_name = country.continent;

            console.log("Country:"+country_name);


            let gm_query = encodeURI(" "+capital_name+", "+country_name);


            // get 60 restaurants
            let restaurants_arr = await getPlaces(gm_query,"restaurant");
            api_counter++;

            // get 60 cafe
            let cafes_arr = await getPlaces(gm_query,"cafe");
            api_counter++;


            //console.log("restaurants_arr:"+JSON.stringify(restaurants_arr));
            //console.log("cafes_arr:"+JSON.stringify(cafes_arr));

            // merge restaurants and cafes
            all_places_arr = restaurants_arr.concat(cafes_arr);

            //console.log("all_places_arr:"+JSON.stringify(all_places_arr));

            // cafes and resturants exists and merged correctly
            if ( all_places_arr!=null && all_places_arr.length > 0){

              let index = 0;

              // For each cafe/restaurant
              for(place of all_places_arr){

                index++;
                let agg_review_text = "";

                var name = place.name;
                var rating = place.rating;
                var place_id = place.place_id;
                var address = place.formatted_address;
                var long = place.geometry.location.lng;
                var lat =  place.geometry.location.lat;
                var types_arr =  place.types;
                var user_ratings_total = place.user_ratings_total;

                // Here I ignore 75% of the places by not getting thier reviews
                // This was decided to save API costs since 25% of places each having 5 reviews should be enough
                if ( index%4!=0){
                  //do nothing
                  console.log("*** Ignoring items not divisible by 4 - to save cost")

                }
                // Place not ignored - 25%
                else
                {





                    console.log("Getting Reviews of place_id:"+place_id);


                    // Get reviews
                    let reviews_list = await getReviews(place_id);

                    api_counter++;

                    // Reviews found for place
                    if (reviews_list !==false && reviews_list !==0){


                      // Aggregate all 5 reviews in 1 string
                      for(review_item of reviews_list){

                        let review_text = review_item.text;

                        agg_review_text = agg_review_text+" "+review_text;

                      }





                    }

                  }


                // Create a new line in our output CSV file
                let csv_line = clean_n_quote(continent_name)+","+clean_n_quote(country_name)+","+clean_n_quote(capital_name)+","+
                               clean_n_quote(place_id)+","+ clean_n_quote(name)+","+clean_n_quote(types_arr[0])+","+clean_n_quote(rating)+","+
                               clean_n_quote(user_ratings_total)+","+ clean_n_quote(agg_review_text)+"\n";

                console.log("CSV Line:"+csv_line)

                //Append line to file
                writeCSVtoFile(output_file_path,csv_line);

              }

            }


      }catch(e){
        console.error("Error:"+e);

      }


      // only try one city
      //break;

      console.log("api counter:"+api_counter)
    }




}


main();
