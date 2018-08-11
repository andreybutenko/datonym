<template>
  <div class="container is-spaced">
    <h1 class="title">Learn about {{ displayName }}</h1>
    <grid-loader :loading="loading"></grid-loader>

    <div class="box features-box" v-if="!loading">
      <b>Popularity</b>
      <p class="is-spaced" v-if="features.classic || displayAllProperties">
        {{ displayName }} is a classic! 🏛️ It has a higher classic score than
        <b>{{ getPercentile('classic.score') }}</b> of
        {{ getPercentileGender('classic.score' )}} names.<br/>
        <i>
          The classic score is calculated by comparing how popular the name was before and after
          the year 2000.
        </i>
      </p>

      <p class="is-spaced" v-if="features.trendy || displayAllProperties">
        {{ displayName }} is trendy! 🔥🔥🔥 It has a higher trendiness score than
        <b>{{ getPercentile('trendiness.score') }}</b> of
        {{ getPercentileGender('trendiness.score' )}} names.<br/>
        <i>
          The classic score is calculated by comparing how popular the name was before and after
          the year 2015.
        </i>
      </p>

      <p v-if="features.popular || displayAllProperties">
        {{ displayName }} is popular! 🤩
        Our database indicates <b>{{ getStat('count') }}</b> people have had
        that name, making it more popular than
        <b>{{ getPercentile('count') }}</b> of names!
      </p>
    </div>

    <div class="box features-box" v-if="!loading">
      <b>Features</b>
      <p v-if="features.balanced || displayAllProperties">
        {{ displayName }} is balanced! ⚖️ It is
        <b>{{ formatPercentage(stats['symmetry']) }}</b> symmetrical, making it more symmetrical
        than <b>{{ getPercentile('symmetry') }}</b> of names.
      </p>

      <p v-if="features.long || displayAllProperties">
        {{ displayName }} is long! 😁 It is longer than
        <b>{{ getPercentile('length') }}</b> of names.
      </p>
      <p v-if="features.short || displayAllProperties">
        {{ displayName }} is short! 😁 It is shorter than
        <b>{{ getPercentile('length', true) }}</b> of names.
      </p>

      <p v-if="features.vowels || displayAllProperties">
        {{ displayName }} is full of vowels! 😃 It has more vowels than
        <b>{{ getPercentile('vowel.score') }}</b> of names.
      </p>

      <p v-if="features.doubly || displayAllProperties">
        {{ displayName }} is pretty doubly! 😉 It has <b>{{ 2 * stats['doubleness'] }}</b>
        back-to-back repeated letters.
      </p>
    </div>

    <div class="box features-box" v-if="!loading">
      <b>Gender Distribution</b>
      <p v-if="features.androgynous || displayAllProperties">
        {{ displayName }} is pretty androgynous! ☿️
        It is only {{ formatPercentage(genderDisparity.percentage) }}
        more popular in {{ genderDisparity.moreGender }}s than {{ genderDisparity.lessGender }}s,
        making it more androgynous than <b>{{ getPercentile('androgynity') }}</b> of names.
      </p>

      <p v-if="features.gendered || displayAllProperties">
        {{ displayName }} is pretty
        {{ genderDisparity.moreGender == 'male' ? 'masculine' : 'feminine' }}!
        {{ genderDisparity.moreGender == 'male' ? '♂️' : '♀️' }}
        It is <b>{{ formatPercentage(genderDisparity.percentage) }}</b>
        more popular in {{ genderDisparity.moreGender }}s than {{ genderDisparity.lessGender }}s.
      </p>
      </div>

    <table v-if="!loading" class="table">
      <thead>
        <tr>
          <th>Property</th>
          <th>Value</th>
        </tr>
      </thead>
      <tbody>
          <tr v-for="stat in iterableStats" :key="stat.key">
            <td>{{ stat.key }}</td>
            <td>{{ stat.val }}</td>
          </tr>
        </tbody>
    </table>
  </div>
</template>

<script>
  import GridLoader from 'vue-spinner/src/GridLoader.vue';
  import axios from 'axios';

  export default {
    name: 'Name',
    components: {
      GridLoader,
    },
    props: {
      name: String,
    },
    data() {
      return {
        loading: true,
        stats: [],
        trends: {},
        displayAllProperties: false,
      };
    },
    mounted() {
      this.loadData();
    },
    methods: {
      loadData() {
        this.loading = true;
        axios.get(`http:///localhost:4500/name/${this.name}`)
          .then((response) => {
            this.loading = false;

            if(response.status === 200) {
              // eslint-disable-next-line
              this.stats = response.data.stats[0]; // TODO we shouldn't have indexes
              this.trends = response.data.trends;
            }
            else {
              alert(`There was an error while getting name data: ${response.statusText}`);
            }
          })
          .catch((error) => {
            this.loading = false;
            const { response } = error;

            alert(`There was an error while getting name data: ${response.statusText}`);
        });
      },
      // Sum the gendered data for a given stat to get the total across genders.
      getStat(stat) {
        return this.formatNumber((this.stats[stat] || 0) +
          (this.stats[`${stat}.m`] || 0) +
          (this.stats[`${stat}.f`] || 0)); // eslint-disable-line
      },
      // Get the highest gendered percentile for a given stat.
      getUnformattedPercentile(stat, invert) {
        const key = `${stat}.perc`;
        let percentile = 0;

        if(Object.keys(this.stats).includes(key)) {
          percentile = this.stats[key];
        }
        else {
          percentile = Math.max(
            this.stats[`${key}.m`],
            this.stats[`${key}.f`],
          );
        }

        return invert ? 1 - percentile : percentile;
      },
      getPercentile(stat, invert) {
        return this.formatPercentage(this.getUnformattedPercentile(stat, invert));
      },
      // Get the gender with a higher percentile for a given stat.
      getPercentileGender(stat) {
        if(Object.keys(this.stats).includes(stat)) {
          return '';
        }

        return this.stats[`${stat}.perc.m`] > this.stats[`${stat}.perc.f`] ?
          'male' : 'female';
      },
      // Add commas in thousands places.
      formatNumber(number) {
        return number.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ',');
      },
      // Format fractions as percentages.
      formatPercentage(perc) {
        return `${Math.round(perc * 100)}%`;
      },
    },
    computed: {
      displayName() {
        return this.name.charAt(0).toUpperCase() + this.name.substr(1).toLowerCase();
      },
      iterableStats() {
        return Object.keys(this.stats).map(key => ({
          key,
          val: this.stats[key],
        }));
      },
      genderDisparity() {
        if(Object.keys(this.stats).includes('count')) {
          return {
            percentage: 100,
            moreGender: 'male',
            lessGender: 'female',
          };
        }

        return {
          percentage: (Math.max(this.stats['count.m'], this.stats['count.f']) -
            Math.min(this.stats['count.m'], this.stats['count.f'])) /
            Math.max(this.stats['count.m'], this.stats['count.f']),
          moreGender: this.stats['count.m'] > this.stats['count.f'] ?
            'male' : 'female',
          lessGender: this.stats['count.m'] < this.stats['count.f'] ?
            'male' : 'female',
        };
      },
      features() {
        console.log(this.genderDisparity.percentage, 85, this.genderDisparity.percentage > 85);

        return {
          classic: this.getUnformattedPercentile('classic.score') > 0.75,
          trendy: this.getUnformattedPercentile('trendiness.score') > 0.75,
          popular: this.getUnformattedPercentile('count') > 0.75,

          balanced: this.stats.symmetry > 0.75,
          long: this.getUnformattedPercentile('length') > 0.75,
          short: this.getUnformattedPercentile('length', true) > 0.75,
          vowels: this.stats['vowel.score'] > 0.75,
          doubly: this.stats.doubleness > 1,

          androgynous: this.getUnformattedPercentile('androgynity') > 0.95,
          gendered: this.genderDisparity.percentage > 0.85,
        };
      },
    },
    watch: {
      $route(to) {
        if(to.name === 'name' && to.params && to.params.name !== this.searchText) {
          this.searchText = to.params.name;
          this.loadData();
        }
      },
    },
  };
</script>

<style scoped lang="scss">
  .features-box p.is-spaced:not(:last-child) {
    margin-bottom: 0.5em;
  }
</style>
