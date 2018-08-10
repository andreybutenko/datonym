<template>
  <div class="container is-spaced">
    <h1 class="title">{{ displayName }}</h1>
    <grid-loader :loading="loading"></grid-loader>

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
        properties: {
          syllables: {
            label: 'Syllables',
          },
          symmetry: {
            label: 'Symmetry',
            type: 'percentage',
          },
          masculinity: {
            label: 'Masculinity',
            type: 'percentage',
          },
          androgynity: {
            label: 'Androgynity',
            type: 'percentage',
          },
          doubleness: {
            label: 'Doubleness',
            type: 'percentage',
          },
        },
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
    },
    computed: {
      displayName() {
        return this.name.charAt(0).toUpperCase() + this.name.substr(1).toLowerCase();
      },
      iterableStats() {
        if(this.displayAllProperties) {
          return Object.keys(this.stats).map(key => ({
            key,
            val: this.stats[key],
          }));
        }

        return Object.keys(this.stats)
          .filter(key => Object.keys(this.properties).includes(key))
          .map(key => ({
            key: this.properties[key].label,
            val: this.properties[key].type === 'percentage' ?
              `${Math.floor(this.stats[key] * 100)}%` :
              this.stats[key],
        }));
      },
    },
    watch: {
      $route(to) {
        if(to.name === 'name' && to.params && to.params.name !== this.searchText) {
          this.searchText = to.params.name;
        }
      },
    },
  };
</script>

<style scoped lang="scss">
</style>
